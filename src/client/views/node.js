sc.views.Node = Dance.Performer.extend(_.extend({}, s.StateMachine, {

  className: 'content-node',

  attributes: {
    draggable: 'false'
  },

  initialize: function (options) {
    this.state  = 'read';
    $(this.el).attr({ id: _.htmlId(this.model) });
  },

  transitionTo: function (state) {
    StateMachine.transitionTo.call(this, state);
    if (this.state === state) {
      this.afterControls.transitionTo(state);
    }
  },


  // Events
  // ------

  events: {
    'click .remove-node':      'removeNode',
    'click .toggle-move-node': 'toggleMoveNode',
    
    'click': 'selectThis',
    'mouseover': 'highlight',
    'mouseout': 'unhighlight'
  },
  
  removeNode: function (e) {
    e.preventDefault();
    e.stopPropagation();
    removeChild(this.parent, this.model);
  },

  toggleMoveNode: function (e) {
    e.preventDefault();
    e.stopPropagation();
    
    if (this.state === 'move') {
      this.root.transitionTo('write');
    } else {
      // There could be another node that is currently in move state.
      // Transition to read state to make sure that no node is in move state.
      this.root.transitionTo('read');
      this.transitionTo('move');
      
      this.root.movedNode = this.model;
      this.root.movedParent = this.parent;
      this.root.transitionTo('moveTarget');
    }
  },

  selectThis: function (e) {
    this.el
  },

  highlight: function (e) {
    e.preventDefault();
    $(this.el).addClass('active');
  },

  unhighlight: function (e) {
    e.preventDefault();
    $(this.el).removeClass('active');
  },

  select: function (e) {
    $(this.el).addClass('selected');
  },

  deselect: function () {
    $(this.el).removeClass('selected');
  },

  focus: function () {},

  render: function () {
    this.contentEl = $('<div class="content"></div>').appendTo(this.el);
    return this;
  }

}), {


  // States
  // ------

  states: {
    read: {
      enter: function () {},
      leave: function () {}
    },
    
    write: {
      enter: function () {},
      leave: function () {}
    },

    move: {
      enter: function () {
        $(this.el).addClass('being-moved'); // TODO
      },
      leave: function (nextState) {
        if (nextState === 'moveTarget') { return false; }
        $(this.el).removeClass('being-moved'); // TODO
      }
    },

    moveTarget: {
      enter: function () {},
      leave: function () {}
    }
  },


  // Inheritance & Instantiation
  // ---------------------------

  subclasses: {},

  define: function (types, protoProps, classProps) {
    classProps = classProps || {};
    var subclass = this.extend(protoProps, classProps);
    
    function toArray (a) { return _.isArray(a) ? a : [a] }
    _.each(toArray(types), function (type) {
      this.subclasses[type] = subclass;
    }, this);
    
    return subclass;
  },

  create: function (options) {
    var model = options.model
    ,   type = model.type()._id
    ,   Subclass = this.subclasses[type];
    
    if (!Subclass) { throw new Error("Node has no subclass for type '"+type+"'"); }
    return new Subclass(options);
  }

});