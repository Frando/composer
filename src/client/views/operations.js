sc.views.Operations = Dance.Performer.extend({

  // Events
  // ------

  events: {

  },

  // Handlers
  // --------

  render: function () {
    this.$el.html(_.tpl('operations', this.model));
    return this;
  }
});