Substance Composer
=====================

The Substance Composer is an open source effort dedicated to the development of a truly flexible editing component to be used by applications such as Substance.io for collaborative content composition. It uses operations to transform documents. By keeping track of atomic document operations, we can replay the complete history and go back and forth in time.


API Appetizer
=====================

```js
var op1 = {"_rev": 1, "command": "insert-node", params: {"type": "section", after: "/text/1"}}

composer.document.rev // => 1
composer.execute(op1);
composer.document.rev // => 2

var op2 = {"command": "insert-comment", params: {
  referenced_nodes: ["/text/1", "/section"],
  "content": "There's a typo in line 5 at the first paragraph."},
  "user": {username: "john", "name": "John Doe"}
};

composer.execute(op2);
composer.document.rev // => 3

var op3 = {"command": "update-node", "params": {"node": "/text/1", "operation": "ret(15) del('abc') ins('ABC') ret(241)"}}

composer.execute(op3); // just transforms the content of text nodes using OT.
```