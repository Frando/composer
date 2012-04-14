sc.instructors.Instructor = Dance.Instructor.extend({
  initialize: function() {

    // Using this.route, because order matters
    this.route('', 'loadDocument', this.loadDocument);
  },

  loadDocument: function() {
    
  }
});