sc.views.Node.define([ '/type/cover' ], {

  className: 'content-node document',

  initialize: function (options) {
    sc.views.Node.prototype.initialize.apply(this, arguments);
  },

  events: _.extend({
  }, sc.views.Node.prototype.events),


  transitionTo: function (state) {
    StateMachine.transitionTo.call(this, state);
    if (this.state === state) {
      this.nodeList.transitionTo(state);
    }
  },

  render: function () {
    sc.views.Node.prototype.render.apply(this, arguments);
    this.titleEl = $('<div class="document-title">'+this.model.get('title')+'</div>').appendTo(this.contentEl);
    this.leadEl = $('<p class="lead" id="document_lead">'+this.model.get('abstract')+'</p>').appendTo(this.contentEl);
    return this;
  }
}, {

  states: {
    write: {
      enter: function () {
        s.views.Node.states.write.enter.apply(this);
        $(this.el).addClass('edit');
      },
      leave: function () {
        s.views.Node.states.write.leave.apply(this);
        $(this.el).removeClass('edit');
        window.editor.deactivate();
      }
    },
    moveTarget: {
      enter: function () {
        $('#document').addClass('move-mode');
      },
      leave: function () {
        delete this.movedNode;
        delete this.movedParent;
        $('#document').removeClass('move-mode');
      }
    }
  }
});