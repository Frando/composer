sc.views.Document = Dance.Performer.extend({
  id: 'document',

  initialize: function (options) {
    _.bindAll(this);

    // It's a stub
    this.model = {"type": {_id: "/type/document"}};
    this.node = sc.views.Node.create({ model: this.model, document: this });

    // this.documentLens = new s.views.DocumentLens({ model: {items: sections, document: this}, authorized: this.authorized });
    
    // this.currentView = null;
    
    // _.bindAll(this, 'deselect', 'onKeydown');
    // $(document.body).keydown(this.onKeydown);
    // if (!this.bounds) setTimeout(this.calcBounds, 400);
    // $(window).scroll(this.markActiveSection);
  },

  render: function () {

    // $(this.el).html(s.util.tpl('document', {
    //   doc: this.model,
    //   authorized: this.authorized
    // }));
    console.log('rendering document');
    
    // // $(this.documentLens.render().el).appendTo(this.$('#document'));
    // $(this.node.render().el).appendTo(this.$('#document'));
    // if (this.authorized && !this.version) { this.edit(); }
    // this.$('#document_content').click(this.deselect);

    // return this;
  },

  remove: function () {
    // $(document.body).unbind('keydown', this.onKeydown);
    // this.$('#document_content').unbind('click', this.deselect);
    // $(this.el).remove();
    // return this;
  },


  // Events
  // ------

  events: {

  },

  // Handlers
  // --------


  onKeydown: function (e) {
    if (e.keyCode === 27) {
      // Escape key
      this.deselect();
    }
  },


  // Helpers
  // -------

  edit: function () {
    // this.node.transitionTo('write');
  },

  deselect: function () {
    // if (this.node) {
    //   this.node.deselectNode();
    //   window.editor.deactivate();
    //   this.$(':focus').blur();
    // }
  }

  // Calculate section bounds
  // calcBounds: function() {
  //   var that = this;
  //   this.bounds = [];
  //   this.sections = [];
  //   $('#document .content-node.section').each(function() {
  //     that.bounds.push($(this).offset().top);
  //     that.sections.push(graph.get(this.id.replace(/_/g, '/')));
  //   });
  // },

  // markActiveSection: function() {
  //   var that = this;
  //   function getActiveSection() {
  //     var active = 0;
  //     _.each(that.bounds, function(bound, index) {
  //       if ($(window).scrollTop() >= bound-90) {
  //         active = index;
  //       }
  //     });
  //     return active;
  //   }

  //   function update(e) {
  //     that.activeSection = getActiveSection();
  //     if (that.activeSection !== that.prevSection) {
  //       that.prevSection = that.activeSection;
  //       that.documentLens.selectSection(that.activeSection);
  //     }
  //   }
  //   update();
  // },
});