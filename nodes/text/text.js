sc.views.Node.define('/type/text', {

  className: 'content-node text',

  focus: function () {
    $(this.textEl).click();
  },

  select: function () {
    sc.views.Node.prototype.select.apply(this);
  },

  deselect: function () {
    sc.views.Node.prototype.deselect.apply(this);
  },

  render: function () {
    sc.views.Node.prototype.render.apply(this, arguments);
    $(this.contentEl).html(this.model.get('content'));
    $(this.contentEl).attr('contenteditable', true);
    return this;
  }
});