(function() {

  suite('Composer', function(){
    setup(function() {
      // ...
    });

    suite('#Document API', function() {
      var document;

      setup(function() {
        document = new Document();
      });

      test('should create a section', function() {
        var section = document.create('section', 'after', '/text/1');
      });

      test('should create an image', function() {
        document.create('section', {name: 'Hello World'}, 'after', '/text/1');
      });

      test('should update text', function() {
        document.update('/text/1', "ret(5) del(2) ret(4)");
      });
      
    });
  });
}).call(this);
