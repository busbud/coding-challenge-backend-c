define(function(require) {

    var Backbone = require('backbone');
    var Mustache = require('mustache');
    var template = require('text!../../templates/suggestion.mustache');
    var $ = require('jquery');
    var _ = require('underscore');
    
    return Backbone.View.extend({
        
        template: template,

        events: {
            'click': 'goMaps'
        },
        
        initialize: function() {
            this.model.maps_url = 'http://maps.google.com/maps?z=16&t=m&q=loc:' + 
                this.model.latitude + '+' +
                this.model.longitude;
        },
        
        render: function () {
            var self = this;
            
            _.each(this.model, function(val, key) {
                if (!isNaN(val)) {
                    self.model[key] = val.toFixed(3);
                }
            });
            
            return Mustache.to_html(template, this.model);
        }
    });

});
