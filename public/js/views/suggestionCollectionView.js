define(function(require) {

    var Backbone = require('backbone');
    var Mustache = require('mustache');
    var template = require('text!../../templates/suggestionCollection.mustache');
    var $ = require('jquery');
    var SuggestionView = require('views/suggestionView');
    
    return Backbone.View.extend({

        template: template,
        
        initialize: function(options) {
            this.suggestions = options.suggestions;
        },
        
        render: function(suggestions) {
            var rendered = Mustache.to_html(this.template);
            
            this.$el.html(rendered);
            
            this.suggestions && this.suggestions.forEach(function(suggestion) {
                var suggestionView = new SuggestionView({model: suggestion})
                $('tbody').append(suggestionView.render());
            });            
        }

    });
});