$(function() {

  var commands = [
    {"command": "insert", "params": {"type": "text", "rev": 2, "attributes": {"content": "I'm a new text node"}}},
    {"command": "insert", "params": {"type": "section", "rev": 3, "attributes": {"name": "It's magical"}}},
    {"command": "insert", "params": {"type": "text", "rev": 4, "attributes": {"content": "Isn't it?"}}}
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