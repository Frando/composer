// Helpers
// ---------------

s.util = {};

// A fake console to calm down some browsers.
if (!window.console) {
  window.console = {
    log: function(msg) {
      // No-op
    }
  };
}

// Render Underscore templates
_.tpl = function (tpl, ctx) {
  var source = templates[tpl];
  return _.template(source, ctx);
};


_.htmlId = function(node) {
  node = node instanceof Data.Object ? node._id : node;
  return node.split('/').join('_');
};