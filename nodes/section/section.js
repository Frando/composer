sc.views.Node.define('/type/section', {

  className: 'content-node section',

  initialize: function (options) {
    sc.views.Node.prototype.initialize.apply(this, arguments);
  },

  focus: function () {
    this.headerEl.click();
  },

  remove: function () {
    this.nodeList.remove();
    $(this.el).remove();
  },

  transitionTo: function (state) {
    sc.views.Node.prototype.transitionTo.call(this, state);
    if (this.state === state) {
      this.nodeList.transitionTo(state);
    }
  },

  render: function () {
    sc.views.Node.prototype.render.apply(this, arguments);
    $(this.contentEl).html(this.model.get('name'));
    return this;
  }
});