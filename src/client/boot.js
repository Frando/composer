$(function() {

  var commands = [
    {"command": "user:announce", "params": {"user": "michael", "color": "#82AA15"}},    
    // {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 3, "attributes": {"content": "It's literally impossible to build an editor that can be used across different disciplines. Scientists, writers and journalists all have different needs. That's why Substance just provides the core infrastructure, and introduces Content Types that can be developed individually by the community, tailored to their specific needs."}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "map", "rev": 3, "attributes": {"content": "Hey! I'm a map."}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "section", "rev": 4, "attributes": {"name": "Structured Composition"}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 5, "attributes": {"content": "Instead of conventional sequential text-editing, documents are composed of Content Nodes in a structured manner. The composer focuses on content, by leaving the layout part to the system, not the user. Because of the absence of formatting utilities, it suggests structured, content-oriented writing."}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "section", "rev": 6, "attributes": {"name": "Open Collaboration"}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 7, "attributes": {"content": "The Substance Composer targets open collaboration. Co-authors can edit one document at the same time, while content is synchronized among users in realtime. There's a strong focus on reader collaboration as well. They can easily participate and comment on certain text passages or suggest a patch."}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "section", "rev": 8, "attributes": {"name": "Patches"}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 9, "attributes": {"content": "Readers will be able to contribute right away by submitting patches, which can be applied to the document at a later time. Patches are an important concept to realize a peer-review process."}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "section", "rev": 10, "attributes": {"name": "Operations"}}},
    {"command": "node:insert",   "params": {"user": "michael", "type": "text", "rev": 11, "attributes": {"content": "The Substance Composer uses atomic operations to transform documents. This is a fundamental concept that allows collaborative editing of one document (even at the same time). The technique behind it is called Operational Transformation. Based on all recorded operations, the complete document history can be reproduced at any time. In other words. This is the best thing since sliced bread."}}},
    {"command": "user:announce", "params": {"user": "john", "color": "#4da6c7"}},
    {"command": "node:select",   "params": {"user": "john", "nodes": ["/cover/1"], "rev": 12}},
    {"command": "node:select",   "params": {"user": "michael", "nodes": ["/section/2", "/text/3"], "rev": 12}},
    {"command": "node:move",     "params": {"user": "michael", "nodes": ["/section/2", "/text/3"], "target": "/text/5", "rev": 12}}
  ];

  // Executes commands in serial
  function execCommands() {
    var index = 0;
    function next() {
      if (index >= commands.length) return;
      composer.execute(commands[index]);
      index += 1;
      _.delay(next, 500);
    }
    _.delay(next, 500);
  }

  sc.models.Document.load("example.json", function(err, doc) {
    window.composer = new Substance.Composer({model: doc, el: '#container', user: "michael"});
    composer.start();

    execCommands();
  });

});