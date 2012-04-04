Substance Composer
=====================

The Substance Composer is an open source effort dedicated to the development of a truly flexible editing component to be used by applications such as Substance.io for collaborative content composition.


Commands
=====================

```js
// Node construction API, attach new empty nodes to the document
var txt = document.insert('text'); // returns a TextNode object

document.update(txt._id, 'ins("Hello World")');

// Insert Section

var section = document.create('section');

document.update(section._id, 'ins("Introduction")');

document.create('section', {name: 'Hello World'}, 'after', '/text/1');

// Image API

var img = document.create('image', 'inside', section._id);

document.update('image', {url: "http://foo.com/bar.png"});

// Update Text
document.update('/text/1', "ret(5) del(2) ret(4)");
```