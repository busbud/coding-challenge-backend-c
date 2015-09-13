"Use Strict";

var prime = require('prime');
var onDomReady = require('elements/domready');

var $$ = require("elements");
require('elements/attributes');
require('elements/events');
require('elements/delegation');
require('elements/insertion');
require('elements/traversal');
require('elements-util/lib/empty');

var zen = require('elements/zen')
var number = require('prime/shell/number');

var mixin = require('prime-util/prime/mixin');
var bound = require('prime-util/prime/bound');

var FuzzySearch = require('../../js/FuzzySearch');
var LevenshteinFS = require('../../js/modules/LevenshteinFS');
var Sift3FS = require('../../js/modules/Sift3FS');
var IndexOfFS = require('../../js/modules/IndexOfFS');
var WordCountFS = require('../../js/modules/WordCountFS');

var Arr = require('prime/es5/array');

onDomReady(function() {
    new Main();
});

var Main = prime({

    constructor: function() {
        this.searchField = $$('#searchfield');

        this.fuzzySearch = new FuzzySearch(fsData, {'caseSensitive': false, 'termPath': ''});
        this.fuzzySearch.addModule(LevenshteinFS({'maxDistanceTolerance': 3, 'factor': 3}));
        this.fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        this.fuzzySearch.addModule(WordCountFS({'maxWordTolerance': 3, 'factor': 1}));

        $$('#maxscore').text(this.fuzzySearch.getMaximumScore());

        this.loadEvents();
        this.displayData();
    },

    loadEvents: function() {
        this.searchField.on('keyup', this.bound('search'));
    },

    displayData: function() {
        var container = $$('#data');
        Arr.forEach(fsData, function(data) {
            zen('li').text(data).insert(container);
        });
    },

    displayResults: function(results, container) {
        var container = container;
        $$(container).empty();


        Arr.forEach(results, function(result) {
            zen('li').text(result.value + ' (total: '+number.round(result.score)+')').insert(container);
        });
    },

    search: function() {
        var term = this.searchField.value();
        var levResults = this.fuzzySearch.search(term);

        this.displayResults(levResults, $$('#results'));
    }


});

mixin(Main, bound);