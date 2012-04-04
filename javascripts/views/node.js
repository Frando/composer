s.views.Node = Backbone.View.extend(_.extend({}, StateMachine, {

  className: 'content-node',

  attributes: {
    draggable: 'false'
  },

  initialize: function (options) {
    this.state  = 'read';
    this.parent = options.parent;
    this.level  = options.level;
    this.root   = options.root;

    if (!this.root) {
      this.nodes = {};
      this.root = this;
      this.document = options.document;
    } else {
      this.root.nodes[this.model._id] = this;
    }

    this.comments = new s.views.Comments({ model: this.model, node: this });
    this.afterControls = new s.views.Controls({
      root: this.root,
      level: this.level,
      model: this.parent,
      position: new Position(this.parent, this.model)
    });
    
    $(this.el).attr({ id: this.model.html_id });
    
    _.bindAll(this, 'lastChildChanged');
    this.model.bind('last-child-changed', this.lastChildChanged);
  },

  transitionTo: function (state) {
    StateMachine.transitionTo.call(this, state);
    if (this.state === state) {
      this.afterControls.transitionTo(state);
    }
  },

  lastChildChanged: function () {
    this.afterControls.render();
    
    if (this.parent && isLastChild(this.parent, this.model)) {
      this.parent.trigger('last-child-changed');
    }
  },



  // Events
  // ------

  events: {
    'click .toggle-comments':  'toggleComments',
    'click .remove-node':      'removeNode',
    'click .toggle-move-node': 'toggleMoveNode',
    
    'click': 'selectThis',
    'mouseover': 'highlight',
    'mouseout': 'unhighlight'
  },

  toggleComments: function (e) {
    e.preventDefault();
    e.stopPropagation();
    this.selectThis();
    this.comments.toggle();
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
    // the parent view shouldn't deselect this view when the event bubbles up
    if (e) { e.stopPropagation(); }
    
    if (this.root) {
      this.root.selectNode(this);
    } else {
      this.selectNode(this);
    }
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

  makeEditable: function (el, attr, dflt, options, updateFn) {
    dflt = dflt || '';
    options = _.extend({
      placeholder: dflt,
      markup: false,
      multiline: false,
      codeFontFamily: 'Monaco, Consolas, "Lucida Console", monospace'
    }, options || {});
    updateFn = updateFn || function (node, attr, val, direction) {
      var update = {};
      update[attr] = val;
      update["direction"] = direction;
      updateNode(node, update);
    };
    
    var self = this;
    
    var value = this.model.get(attr);
    if (value) {
      if (options.markup) {
        $(el).html(value);
      } else {
        $(el).text(s.util.unescape(value));
      }
    } else {
      $(el).html('&laquo; '+dflt+' &raquo;').addClass('empty');
    }
    
    $(el)
      .addClass('editable')
      .click(function () {
        if (self.state === 'write') {
          window.editor.activate($(el), options);
          window.editor.bind('changed', function () {
            updateFn(self.model, attr, window.editor.content(), window.editor.direction());
          });
        }
      });
    
    return $(el);
  },

  render: function () {
    this.operationsEl = $(s.util.tpl('operations', {
      commentCount: this.model.get('comment_count') || "",
      authorized: this.root.document.authorized
    })).appendTo(this.el);
    this.contentEl = $('<div class="content"/>').appendTo(this.el);
    if (this.model.get('direction') === "right") this.contentEl.css('direction', 'rtl');
    if (this.comments) this.commentsEl = $(this.comments.render().el).appendTo(this.el);
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
    ,   type = model.type._id
    ,   Subclass = this.subclasses[type];
    
    if (!Subclass) { throw new Error("Node has no subclass for type '"+type+"'"); }
    return new Subclass(options);
  }

});