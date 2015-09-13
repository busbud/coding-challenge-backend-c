"Use Strict";

var prime = require('prime');
var trim = require('prime/string/trim');

var arr = {
	'forEach': require('prime/array/forEach')
};

var obj = {
	'mixin': require('prime/object/mixIn'),
	'fromPath': require('./fromPath'),
	'create': require('prime/object/create')
};


var FuzzySearch = prime({

    modules: null,

    options: {
        'caseSensitive': false,
        'termPath': '',
        'returnEmptyArray': false,
        'minimumScore': 0
    },

    constructor: function(searchSet, options) {
        this.options = obj.mixin(obj.create(this.options), options);
        this.searchSet = searchSet;
        this.modules = [];
    },

    addModule: function(mod) {
        this.modules.push(mod);
    },

    search: function(needle) {
        needle = !this.options.caseSensitive ? trim(needle).toLowerCase() : trim(needle);
        var result = [];

        arr.forEach(this.searchSet, function(value) {
            var origValue = value;
            var searchValue = this.options.termPath.length === 0 ? value : obj.fromPath(value, this.options.termPath);

            if (!this.options.caseSensitive) {
                searchValue = searchValue.toLowerCase();
            }

            var score = this.getCombinedModulePoints(needle, searchValue);

            if (score.combined >= this.options.minimumScore) result.push({'score': score.combined, 'details': score.details, 'value': origValue});
        }, this);

        if (!this.options.returnEmptyArray && result.length === 0) {
            return null;
        }

        return result.sort(function(a, b) {
            return b.score - a.score;
        });
    },

    getCombinedModulePoints: function(needle, haystack) {
        var result = {'combined': 0, 'details': []};
        arr.forEach(this.modules, function(mod) {
            var score = mod.search(needle, haystack).getPoints();
            var name = mod.getName();
            var factor = mod.getFactor();

            result.combined += factor * score;
            result.details.push({'name': name, 'score': score, 'factor': factor});
        });

        return result;
    },

    getMaximumScore: function() {
        var factorSum = 0;
        arr.forEach(this.modules, function(mod) {
            factorSum += mod.getFactor();
        });

        return 100 * factorSum;
    }

});

module.exports = FuzzySearch;