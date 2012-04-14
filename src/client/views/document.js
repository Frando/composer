sc.views.Document = Dance.Performer.extend({
  id: 'document',

  // Events
  // ------

  events: {

  },

  // Handlers
  // --------

  onKeydown: function (e) {
    if (e.keyCode === 27) this.deselect(); // ESC
  },

  initialize: function (options) {
    _.bindAll(this, 'deselect', 'onKeydown', 'insertNode');

    this.model.on('node:insert', this.insertNode);
    this.model.on('node:update', this.updateNode);

    this.build();
    $(document.body).keydown(this.onKeydown);
  },

  build: function() {
    this.nodes = [];
    this.model.each(function(node) {
      this.nodes.push(this.createNodeView(node));
    }, this);
  },

  insertNode: function(node, options) {
    console.log('inserting node', node);
    var view = this.createNodeView(node);
    this.nodes.push(view);
    $(view.render().el).appendTo(this.el);
  },

  createNodeView: function(node) {
    return sc.views.Node.create({
      document: this.model,
      model: node
    });
  },

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
    _.each(this.nodes, function(node, index) {
      $(node.render().el).appendTo(this.el);
    }, this);
    return this;
  },

  // Helpers
  // -------

  edit: function () {
    this.node.transitionTo('write');
  },

  deselect: function () {
    // TODO
  }

});