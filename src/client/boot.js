$(function() {

  var commands = [
    {"command": "user:announce", "params": {"user": "michael", "color": "#82AA15"}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 3, "attributes": {"content": "I'm a new text node"}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "section", "rev": 4, "attributes": {"name": "Operations"}}},
    {"command": "user:announce", "params": {"user": "john", "color": "#4da6c7"}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 5, "attributes": {"content": "Documents are manipulated through atomic operations."}}},
    {"command": "node:select",   "params": {"user": "john", "nodes": ["/cover/1"], "rev": 5}},
    {"command": "node:select",   "params": {"user": "michael", "nodes": ["/section/2"], "rev": 5}}
  ];

  // Executes commands in serial
  function execCommands() {
    var index = 0;
    function next() {
      if (index >= commands.length) return;
      composer.execute(commands[index]);
      index += 1;
      _.delay(next, 100);
    }
    _.delay(next, 100);
  }

  sc.models.Document.load("example.json", function(err, doc) {
    window.composer = new Substance.Composer({model: doc, el: '#container', user: "michael"});
    composer.start();

    execCommands();
  });

});