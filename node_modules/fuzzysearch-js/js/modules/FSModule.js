"Use Strict";

var prime = require('prime');
var obj = {
	'mixin': require('prime/object/mixIn'),
	'create': require('prime/object/create')
};
var FSModule = prime({

    lastTerm: '',
    lastHaystack: '',
    lastResults: null,

    options: {
        'factor': 1
    },

    constructor: function(options) {
        this.options = obj.mixin(obj.create(this.options), options);
        this.lastResults = [];
    },

    search: function(searchTerm) {
        throw new Error("search method not implemented");
    },

    getPoints: function() {
        throw new Error("getPoints method not implemented");
    },

    getMatches: function() {
        return this.lastResults;
    },

    getFactor: function() {
        return this.options.factor || 1;
    },

    getName: function() {
        if (!this.name) throw new Error("set module name!");

        return this.name;
    }

});

module.exports = FSModule;