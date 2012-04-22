sc.models.Document = function(document) {
  var that = this;

  // Initialize document
  this.nodes = new Data.Graph(seed);
  this.nodes.merge(document.nodes);

  this.head = this.nodes.get(document.head);
  this.tail = this.nodes.get(document.tail);

  this.rev = document.rev;

  this.selections = {};
  this.users = {};

  // Operations History
  this.operations = [];

  function checkRev(rev) {
    return that.rev === rev;
  }

  // Node API
  // --------

  this.node = {

    // Process update command
    update: function(options) {
      var node = {};
      that.trigger('node:update', node);
    },

    // Update selection
    select: function(options) {
      if (that.users[options.user].selection) {
        _.each(that.users[options.user].selection, function(node) {
          delete that.selections[node];
        });
      }

      that.users[options.user].selection = options.nodes;

      _.each(options.nodes, function(node) {
        that.selections[node] = options.user;
      });

      that.trigger('node:select', that.selections);
    },

    // Insert a new node
    insert: function(options) {
      if (checkRev(options.rev)) {
        
        var node = that.nodes.set(_.extend({
          "type": ["/type/node", "/type/"+options.type],
          _id: ["", options.type, options.rev].join('/'),
          prev: that.tail._id
        }, options.attributes));
        that.tail.set({next: node._id});
        that.tail = node;
        if (node) {
          that.rev += 1;
          that.trigger('node:insert', node);
          return node;
        }
      }
      return null;
    },

    // Move selected nodes
    move: function(options) {
      console.log(that.rev);
      if (checkRev(options.rev)) {
        var f = that.get(_.first(options.nodes)), // first node of selection
            l = that.get(_.first(options.nodes)), // last node of selection
            t = that.get(options.target), // target node
            fp = f.get('prev'),
            ln = l.get('next'),
            tn = t.get('next');

        // console.log('before');
        // console.log('f', f.toJSON(), 'l', l.toJSON(), 't', t.toJSON(), 'fp', fp.toJSON(), 'ln', ln.toJSON(), 'tn', tn.toJSON());

        t.set({next: f._id, prev: t.get('prev') === l ? fp._id : t.get('prev')._id});
        fp.set({next: ln._id});
        
        if (ln) ln.set({prev: fp._id});
        l.set({next: tn ? tn._id : null});
        if (tn) tn.set({prev: l._id});
        that.trigger('node:move', options);
        that.rev += 1;

        // console.log('after');
        // console.log('f', f.toJSON(), 'l', l.toJSON(), 't', t.toJSON(), 'fp', fp.toJSON(), 'ln', ln.toJSON(), 'tn', tn.toJSON());
      }
    },

    // Delete node by id
    delete: function(node) {

    }
  };


  // Patch API
  // --------

  this.patch = {

  };

  // Comment API
  // --------

  this.comment = {

  };

  // User API
  // --------

  this.user = {
    // TODO: dynamic color assignment for users
    announce: function(options) {
      that.users[options.user] = { username: options.user, color: options.color || "red"};
    }
  };


  // Document API
  // --------

  // Iterate over all nodes
  this.each = function(fn, ctx) {
    var current = this.head;
    var index = 0;

    fn.call(ctx || this, current, current._id, index);
    while (current = current.get('next')) {
      index += 1;
      fn.call(ctx || this, current, current._id, index);
    }
  };

  this.logOperation = function(op) {
    this.operations.push(op);
    this.trigger('operation:executed');
  };

  this.execute = function(op) {
    var command = op.command.split(':');
    this[command[0]][command[1]](op.params);
    this.logOperation(op);
  };

  // Get a specific node
  this.get = function(id) {
    return this.nodes.get(id);
  };

  // Serialize document state to JSON
  this.toJSON = function() {

  };
};


// Load a document
sc.models.Document.load = function(url, cb) {
  $.getJSON(url, function(data) {
    cb(null, data);
  });
};

_.extend(sc.models.Document.prototype, _.Events);
