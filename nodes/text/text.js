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
    var that = this;
    sc.views.Node.prototype.render.apply(this, arguments);

    setTimeout(function() {
      that.editor = CodeMirror(that.contentEl[0], {
        lineWrapping: true,
        value: that.model.get('content'),
        onChange: function() {
        }
      });      
    }, 20);
    return this;
  }
});