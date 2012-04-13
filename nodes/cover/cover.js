sc.views.Node.define([ '/type/cover' ], {

  className: 'content-node document',

  initialize: function (options) {
    sc.views.Node.prototype.initialize.apply(this, arguments);
    delete this.comments;
    delete this.afterControls;

    // this.nodeList = new s.views.NodeList({
    //   model: this.model,
    //   level: 0,
    //   root: this
    // });
  },

  events: _.extend({
    'mouseover .editable': 'mouseoverEditable'
  }, sc.views.Node.prototype.events),

  mouseoverEditable: function (e) {
    var title = this.state === 'write'
              ? "Click to Edit"
              : "";
    $(e.target).attr({ title: title });
  },

  transitionTo: function (state) {
    StateMachine.transitionTo.call(this, state);
    if (this.state === state) {
      this.nodeList.transitionTo(state);
    }
  },

  lastChildChanged: function () {},

  selectNode: function (view) {
    this.deselectNode();
    $(this.el).addClass('something-selected');
    view.select();
    this.selected = view;
  },

  deselectNode: function () {
    if (this.selected) {
      $(this.el).removeClass('something-selected');
      this.selected.deselect();
      delete this.selected;
    }
  },

  render: function () {
    sc.views.Node.prototype.render.apply(this, arguments);
    this.$('.content-node-outline').remove();
    this.operationsEl.empty();
    
    var creator     = this.model.get('creator')
    ,   publishedOn = this.model.get('published_on');
    
    this.titleEl     = this.makeEditable($('<div class="document-title" />'), 'title', "Enter Title").appendTo(this.contentEl);
    var authorLink = $('<a class="toggle-view" />')
      .attr({ href: '/'+creator.get('username') })
      .text(creator.get('name') || creator.get('username'));
    this.authorEl    = $('<p class="author" />').append(authorLink).appendTo(this.contentEl);
    this.publishedEl = $('<p class="published" />').text(publishedOn ? s.util.date(publishedOn) : '').appendTo(this.contentEl);
    this.leadEl      = this.makeEditable($('<p class="lead" id="document_lead" />'), 'lead', "Enter Lead").appendTo(this.contentEl);
    $('<div class="document-separator" />').appendTo(this.contentEl);
    this.nodeListEl  = $(this.nodeList.render().el).appendTo(this.contentEl);
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