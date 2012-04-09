var express = require('express');
var app = express.createServer();
var http = require('http');
var crypto = require('crypto');
var fs = require('fs');
var url = require('url');
var child_process = require('child_process');
var Data = require('./lib/data/data');
var _ = require('underscore');

// App config
// -----------

global.config = JSON.parse(fs.readFileSync(__dirname+ '/config.json', 'utf-8'));
global.seed = JSON.parse(fs.readFileSync(__dirname+ '/db/schema.json', 'utf-8'));
global.settings = JSON.parse(fs.readFileSync(__dirname+ '/settings.json', 'utf-8'));


// Express.js Configuration
// -----------

app.configure(function() {
  var CookieStore = require('cookie-sessions');
  app.use(express.bodyParser());
  app.use(express.methodOverride());
  app.use(CookieStore({secret: config.secret}));
  app.use(app.router);
  app.use(express.static(__dirname+"/public", { maxAge: 41 }));
  app.use(express.static(__dirname+"/test", { maxAge: 41 }));
  app.use(express.static(__dirname+"/src", { maxAge: 41 }));
  app.use(express.static(__dirname+"/lib", { maxAge: 41 }));
  app.use(express.logger({ format: ':method :url' }));
});


app.enable("jsonp callback");

function serveStartpage(req, res) {
  html = fs.readFileSync(__dirname+ '/layouts/app.html', 'utf-8');

  function serve() {
    getNotifications(req.session.username, function(err, notifications) {
      var sessionSeed = _.extend(_.clone(seed), notifications);
      res.send(html.replace('{{{{seed}}}}', JSON.stringify(sessionSeed))
                   .replace('{{{{scripts}}}}', JSON.stringify(scripts()))
                   .replace('{{{{templates}}}}', JSON.stringify(templates())));
    });
  }
  serve();
}


// Helpers
// -----------

_.escapeHTML = function(string) {
  return string.replace(/&(?!\w+;)/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
};

function isProduction () {
  return process.env.NODE_ENV === 'production';
}

function scripts() {
  if (isProduction()) {
    return settings.scripts.production;
  } else {
    return settings.scripts.development.concat(settings.scripts.source);
  }
}

function loadTemplates() {
  var tpls = {};
  var files = fs.readdirSync(__dirname + '/templates');
  _.each(files, function (file) {
    var name    = file.replace(/\.ejs$/, '')
    ,   content = fs.readFileSync(__dirname + '/templates/' + file, 'utf-8');
    tpls[name] = content;
  });
  return tpls;
}

var styles;
function loadStyles(callback) {
  if (isProduction() && styles) return callback(styles);
  
  var mainFile = __dirname + '/styles/main.less';
  child_process.exec('lessc ' + mainFile, function (error, stdout, stderr) {
    if (error) {
      styles = '/* An error occurred: ' + stderr + ' */';
      console.log(stderr);
    } else {
      styles = stdout;
    }
    callback(styles);
  });
}

var templates = isProduction()
              ? _.once(loadTemplates)
              : loadTemplates;


app.listen(config['server_port'], config['server_host'], function (err) {
  console.log('READY: Substance is listening http://'+config['server_host']+':'+config['server_port']);
});