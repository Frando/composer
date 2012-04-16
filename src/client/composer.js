(function(exports) {

  // The Substance Namespace
  var Substance = {};

  var Composer = Dance.Performer.extend({
    el: 'container',
    initialize: function() {

      // Document Model
      this.model = new Composer.models.Document(this.model);

      // Operations History
      this.operations = [];

      // Views
      this.views = {};
      this.views.document = new Substance.Composer.views.Document({model: this.model});
      
      // Initialize Router
      this.instructor = new Substance.Composer.instructors.Instructor({});
    },


    logOperation: function(op) {
      this.operations.push(op);
      this.trigger('operation:executed');
    },

    // Dispatch Operation
    execute: function(op) {
      var command = op.command.split(':');
      this.model[command[0]][command[1]](op.params);
      this.logOperation(op);
    },

    start: function() {
      Dance.history.start();
      this.render();
    },

    render: function() {
      this.$el.html(_.tpl('composer'));
      this.$('#document').replaceWith(this.views.document.render().el);
    }
  },
  // Class Variables
  {
    models: {},
    views: {},
    instructors: {},
    utils: {}
  });

  // Exports
  Substance.Composer = Composer;
  exports.Substance = Substance;
  exports.s = Substance;
  exports.sc = Substance.Composer;

})(window);