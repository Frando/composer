sc.models.Document = function(document) {
  var that = this;
  // Initialize document
  this.nodes = new Data.Graph(seed);
  this.nodes.merge(document.nodes);

  this.head = this.nodes.get(document.head);
  this.tail = this.nodes.get(document.tail);
  this.rev = document.rev;


  function checkRev(rev) {
    return that.rev === rev;
  }

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

  // Get a specific node
  this.get = function(id) {

  };

  // Process update command
  this.update = function(id, options) {
    var node = {};
    this.trigger('node:update', node);
  };

  // Insert a new node
  this.insert = function(options) {
    if (checkRev(options.rev)) {
      
      var node = this.nodes.set(_.extend({
        "type": ["/type/node", "/type/"+options.type],
        _id: ["", options.type, options.rev].join('/'),
        prev: this.tail._id
      }, options.attributes));
      this.tail.set({next: node._id});
      this.tail = node;
      if (node) {
        this.rev += 1;
        this.trigger('node:insert', node);
        return node;
      }  
    }
    return null;
  };


  // Serialize document state to JSON
  this.toJSON = function() {

  };

  // Delete node by id
  this.delete = function(node) {

  };

  // Move an existing node around
  this.move = function(node, options) {

  };
};

// Load a document
sc.models.Document.load = function(url, cb) {
  $.getJSON(url, function(data) {
    cb(null, data);
  });
};

_.extend(sc.models.Document.prototype, _.Events);
