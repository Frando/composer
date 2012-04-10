var child_process = require('child_process'),
    fs = require('fs'),
    _ = require('underscore');


var util = {};
var settings = JSON.parse(fs.readFileSync(__dirname+ '/../../settings.json', 'utf-8'));

util.escapeHTML = function(string) {
  return string.replace(/&(?!\w+;)/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
};


util.isProduction = function() {
  return process.env.NODE_ENV === 'production';
};


util.scripts = function() {
  var scripts = [];
  if (util.isProduction()) {
    scripts = settings.scripts.production;
  } else {
    scripts = settings.scripts.development.concat(settings.scripts.source);
  }

  // Include nodes
  var files = fs.readdirSync(__dirname + '/../../nodes');
  _.each(files, function (file) {
    if (file.indexOf('.')>=0) return;
    scripts.push(file+"/"+file+".js");
  });
  return scripts;
}


util.loadTemplates = function() {
  var tpls = {};
  var files = fs.readdirSync(__dirname + '/../../templates');
  _.each(files, function (file) {
    var name    = file.replace(/\.ejs$/, '')
    ,   content = fs.readFileSync(__dirname + '/../../templates/' + file, 'utf-8');
    tpls[name] = content;
  });

  // Include node templates
  var files = fs.readdirSync(__dirname + '/../../nodes');
  _.each(files, function (file) {
    if (file.indexOf('.')>=0) return;
    tpls[file] = fs.readFileSync(__dirname + '/../../nodes/' + file + '/' + file + '.ejs', 'utf-8');;
    // scripts.push(file+"/"+file+".js");
  });
  return tpls;
};


util.schema = function() {
  var schema = JSON.parse(fs.readFileSync(__dirname+ '/../../data/schema.json', 'utf-8'));
  var files = fs.readdirSync(__dirname + '/../../nodes');
  _.each(files, function (file) {
    if (file.indexOf('.')>=0) return;
    _.extend(schema, JSON.parse(fs.readFileSync(__dirname + '/../../nodes/' + file + "/schema.json" , 'utf-8')))
  });
  return schema;
};


util.loadStyles = function(cb) {
  if (util.isProduction() && styles) return cb(styles);
  
  var mainFile = 'styles/main.less';
  child_process.exec('node_modules/less/bin/lessc ' + mainFile, function (error, stdout, stderr) {
    if (error) {
      util.styles = '/* An error occurred: ' + stderr + ' */';
      console.log(stderr);
    } else {
      util.styles = stdout;
    }
    cb(util.styles);
  });
};

util.templates = util.isProduction() ? _.once(util.loadTemplates) : util.loadTemplates;

module.exports = util;