sc.instructors.Instructor = Dance.Instructor.extend({
  initialize: function() {

    // Using this.route, because order matters
    // this.route(':indicator', 'indicator', app.instance.indicator);
    // this.route(':indicator/:year', 'indicator', app.instance.indicator);
    // this.route('methodology', 'methodology', app.instance.methodology);
    // this.route('states', 'states', app.instance.states);
    // this.route('states/:state', 'state-profiles', app.instance.stateProfile);
    // this.route('states/:state/:year', 'state-profiles', app.instance.stateProfile);
    this.route('', 'loadDocument', this.loadDocument);
  },

  loadDocument: function() {
    
  }
});