(function(exports) {

  // The Substance Namespace
  var Substance = {};

  var Composer = Dance.Performer.extend({
    initialize: function() {

      // The composer owns a graph
      this.document = new Composer.models.Document();

      // Document View
      this.views = {};

      // this.views.nodes = new Substance.Composer.views.NodeList();
      this.views.document = new Substance.Composer.views.Document();
      
      // Initialize router
      this.instructor = new Substance.Composer.instructors.Instructor({});
    },

    start: function() {
      Dance.history.start();
      this.render();
    },

    render: function() {
      $('#container').html(_.tpl('composer'));
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