(function(exports) {

  // The Substance Namespace
  var Substance = {};

  var Composer = Dance.Performer.extend({
    el: 'container',
    initialize: function() {

      // Document Model
      this.model = new Composer.models.Document(this.model);

      // Views
      this.views = {};
      this.views.document = new Substance.Composer.views.Document({model: this.model});
      this.views.operations = new Substance.Composer.views.Operations({model: this.model});
      

      this.model.on('operation:executed', this.renderOperations, this);
      // Initialize Router
      this.instructor = new Substance.Composer.instructors.Instructor({});
    },

    // Dispatch Operation
    execute: function(op) {
      this.model.execute(op);      
    },

    start: function() {
      Dance.history.start();
      this.render();
    },

    render: function() {
      this.$el.html(_.tpl('composer'));
      this.$('#document').replaceWith(this.views.document.render().el);
    },

    renderOperations: function() {
      this.$('#sidebar').html(this.views.operations.render().el);
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