define(function(require) {

    var Backbone = require('backbone');
    var Mustache = require('mustache');
    var toolbarTemplate = require('text!../../templates/toolbar.mustache');
    var $ = require('jquery');
    
    return Backbone.View.extend({
        template: toolbarTemplate,
        
        render: function () {
            var rendered = Mustache.to_html(toolbarTemplate, this.model);
            this.$el.html(rendered);
        }
    });
});