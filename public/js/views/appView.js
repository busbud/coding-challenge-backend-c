define(function(require) {

    var Backbone = require('backbone');
    var Mustache = require('mustache');
    var template = require('text!../../templates/app.mustache');
    var $ = require('jquery');
    var ToolbarView = require('views/toolbarView');
    var SuggestionCollectionView = require('views/suggestionCollectionView');

    return Backbone.View.extend({
        el: $('body'),

        events: {
            'click #get-suggestions-btn': 'getSuggestions'
        },

        template: template,

        initialize: function() {
            this.render();
        },

        assign: function (view, selector) {
            view.setElement(this.$(selector)).render();
        },

        render: function () {
            var rendered = Mustache.to_html(this.template, this.model);
            this.$el.html(rendered);
            var toolbarView = new ToolbarView();
            this.assign(toolbarView, '#navbar-container');
        },

        getSuggestions: function(e) {
            e.preventDefault();
            var query = $('#suggestion-input').val()
            var lat = $('#latitude-input').val()
            var long = $('#longitude-input').val()
            var self = this;
            
            $.ajax({
                type: 'GET',
                url: '/suggestions',
                data: {
                    q: query,
                    latitude: lat,
                    longitude: long
                },
                success: function(res) {
                    var suggestionCollectionView = new SuggestionCollectionView({
                        suggestions: res.suggestions
                    });
                    self.assign(suggestionCollectionView, '#search-results')
                },
                error: function(res) {
                    if (res.status === 400) {
                        alert(res.responseJSON.error)
                    } 
                    if (res.status === 404) {
                        this.success(res);
                        alert('No results found. Try another search?');
                    }
                },
                dataType: 'json'
            });
        }
    });

});
