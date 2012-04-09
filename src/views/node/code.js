s.views.Node.define('/type/code', {

  className: 'content-node code',

  events: _.extend({
    'change select': 'changeLanguageSelect'
  }, s.views.Node.prototype.events),

  languages: [ 'JavaScript', 'Python', 'Ruby', 'PHP', 'HTML', 'CSS', 'Haskell'
             , 'CoffeeScript', 'Java', 'C', 'C++', 'C#', 'Other'
             ],

  modeForLanguage: function (language) {
    return {
      javascript: 'javascript',
      python: { name: 'python', version: 3 },
      ruby: 'ruby',
      php: 'php',
      html: 'htmlmixed',
      css: 'css',
      haskell: 'haskell',
      coffeescript: 'coffeescript',
      java: 'text/x-java',
      c: 'text/x-csrc',
      'c++': 'text/x-c++src',
      'c#': 'text/x-csharp'
    }[language] || 'null';
  },

  changeLanguageSelect: function () {
    var newLanguage = this.languageSelect.val();
    updateNode(this.model, { language: newLanguage });
    this.codeMirror.setOption('mode', this.modeForLanguage(newLanguage));
  },

  focus: function () {
    this.codeMirror.focus();
  },

  codeMirrorConfig: {
    lineNumbers: true,
    theme: 'elegant',
    indentUnit: 2,
    indentWithTabs: false,
    tabMode: 'shift'
  },

  render: function () {
    function createSelect (dflt, opts) {
      var html = '<select>';
      _.each(opts, function (lang) {
        var value = lang.toLowerCase();
        selected = dflt === value ? ' selected="selected"' : '';
        html += '<option value="' + value + '"' + selected + '>' + lang + '</option>';
      });
      html += '</select>';
      return html;
    }
    
    var self = this;
    
    s.views.Node.prototype.render.apply(this, arguments);
    this.languageSelect = $(createSelect(this.model.get('language'), this.languages)).appendTo(this.contentEl);
    var codeMirrorConfig = _.extend({}, this.codeMirrorConfig, {
      mode: this.modeForLanguage(this.model.get('language')),
      value: s.util.unescape(this.model.get('content') || ''),
      readOnly: true,
      onFocus: function () {
        // Without this, there is the possibility to focus the editor without
        // activating the code node. Don't ask me why.
        self.selectThis();
      },
      onBlur: function () {
        // Try to prevent multiple selections in multiple CodeMirror instances
        self.codeMirror.setSelection({ line:0, ch:0 }, { line:0, ch:0 });
      },
      onChange: _.throttle(function () {
        updateNode(self.model, { content: s.util.escape(self.codeMirror.getValue()) });
      }, 500)
    });
    this.codeMirror = CodeMirror(this.contentEl.get(0), codeMirrorConfig);
    
    setTimeout(function () {
      // after dom insertion
      self.codeMirror.refresh();
    }, 10);
    
    return this;
  }

}, {

  states: {
    write: {
      enter: function () {
        s.views.Node.states.write.enter.apply(this);
        this.codeMirror.setOption('readOnly', false);
      },
      leave: function () {
        s.views.Node.states.write.leave.apply(this);
        this.codeMirror.setOption('readOnly', true);
      }
    }
  }

});