s.views.Node.define('/type/resource', {

  className: 'content-node resource',

  initialize: function () {
    s.views.Node.prototype.initialize.apply(this, arguments);
    this.updateUrl = _.throttle(this.updateUrl, 500);
  },

  focus: function () {
    this.caption.click();
  },

  resourceExists: function (url, callback) {
    var img = new Image();
    img.onload  = function () { callback(true); }
    img.onerror = function () { callback(false); }
    img.src = url;
  },

  updateUrl: function (url) {
    this.resourceExists(url, _.bind(function (doesIt) {
      if (doesIt) {
        console.log("Valid resource: " + url);
        this.img.attr({ src: url });
        this.status.addClass('image').text("Image");
        updateNode(this.model, { url: url });
      } else {
        this.status.prop({ className: 'status' }).text("Invalid URL");
      }
    }, this));
  },

  render: function () {
    s.views.Node.prototype.render.apply(this);
    
    this.resourceContent = $('<div class="resource-content" />').appendTo(this.contentEl);
    if (!this.model.get('url')) { this.resourceContent.addClass('placeholder'); }
    
    this.img = $('<img />')
      .attr({ src: this.model.get('url') || '/images/image_placeholder.png' })
      .appendTo(this.resourceContent);
    
    var resourceEditor = $(s.util.tpl('resource_editor', {})).appendTo(this.contentEl);
    
    this.status = resourceEditor.find('.status');
    
    this.resourceUrl = resourceEditor.find('.resource-url')
      .val(this.model.get('url'))
      .keyup(_.bind(function () {
        this.updateUrl($(this.resourceUrl).val());
      }, this));
    
    this.caption = this.makeEditable($('<div class="caption" />'), 'caption', "Enter Caption")
      .insertAfter(this.contentEl);
    
    return this;
  }

}, {

  states: {
    write: {
      enter: function () {
        s.views.Node.states.write.enter.apply(this);
        this.$('.resource-url').removeAttr('readonly');
      },
      leave: function () {
        s.views.Node.states.write.leave.apply(this);
        this.$('.resource-url').attr({ readonly: 'readonly' });
      }
    }
  }

});