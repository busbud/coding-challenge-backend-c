"Use Strict";

var prime = require('prime');
var FSModule = require('./FSModule');
var number = {
	'limit': require('prime/number/limit')
};

var WordCountFS = prime({

    inherits: FSModule,

    name: 'WordCountFS',
    options: {
        'maxWordTolerance': 3
    },

    search: function(searchTerm, haystack) {
        this.lastTerm = searchTerm;
        this.lastHaystack = haystack;

        return this;
    },

    getPoints: function() {
        var needleWords = this.lastTerm.split(' ');
        var haystackWords = this.lastHaystack.split(' ');

        return 100 / this.options.maxWordTolerance * (this.options.maxWordTolerance - number.limit(Math.abs(haystackWords.length - needleWords.length), 0, this.options.maxWordTolerance));
    }

});

module.exports = function(options) {return new WordCountFS(options);};