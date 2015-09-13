"Use Strict";

var prime = require('prime');
var FSModule = require('./FSModule');
var arr = {
	'forEach': require('prime/array/forEach')
};

var IndexOfFS = prime({

    inherits: FSModule,

    name: 'IndexOfFS',
    options: {
        'minTermLength': 3,
        'maxIterations': 500,
        'factor': 1
    },

    search: function(searchTerm, searchHaystack) {
        this.lastTerm = searchTerm;
        this.lastHaystack = searchHaystack;
        var minLength = searchTerm.length >= this.options.minTermLength ? this.options.minTermLength : searchTerm.length;

        var matches = [];
        var iterations = 0;
        do {
            var cm = this.getClosestMatch(searchTerm, searchHaystack);
            if (cm.length >= minLength) {
                matches.push(cm);
            }

            var substrc = (cm.length - 1 > 0) ? cm.length : 1;
            searchTerm = searchTerm.substr(substrc);
            iterations++;
        } while (searchTerm.length >= minLength && iterations <= this.options.maxIterations);


        this.lastResults = matches;
        return this;
    },

    getClosestMatch: function(searchTerm, haystack) {
        if (haystack.indexOf(searchTerm) != -1) {
            return searchTerm;
        }

        var length = searchTerm.length;

        for (var i = 0; i <= length; i++) {
            var term = searchTerm.substr(0, i);
            if (haystack.indexOf(term) != -1) {
                continue;
            }

            return term.substr(0, i - 1);
        }

        return "";
    },

    getPoints: function() {
        var sum = 0;
        arr.forEach(this.lastResults, function(result) {
            sum += result.length;
        });

        return 100 / this.lastTerm.length * sum;
    }

});

module.exports = function(options) {return new IndexOfFS(options);};