$(function() {
  var commands = [
    {"command": "node:insert", "user": "michael", "params": {"type": "text", "rev": 2, "attributes": {"content": "I'm a new text node"}}},
    {"command": "node:insert", "user": "michael", "params": {"type": "section", "rev": 3, "attributes": {"name": "Operations"}}},
    {"command": "node:insert", "user": "michael", "params": {"type": "text", "rev": 4, "attributes": {"content": "Documents are manipulated through atomic operations."}}},
    {"command": "node:select", "user": "michael", "params": {"nodes": ["/text/2"], "rev": 4}}
  ];

  // Executes commands in serial
  function execCommands() {
    index = 0;
    function next() {
      if (index >= commands.length) return;
      composer.execute(commands[index]);
      index += 1;
      _.delay(next, 500);
    }
    _.delay(next, 500);
  }

  sc.models.Document.load("example.json", function(err, doc) {
    window.composer = new Substance.Composer({model: doc, el: '#container'});
    composer.start();
    execCommands();
  });

  

});