"use strict";

var expect = require('expect.js');
var FuzzySearch = require('../js/FuzzySearch');

var IndexOfFS = require('../js/modules/IndexOfFS');

var dataArr = ['Hydrogen', 'Helium', 'Lithium', 'Beryllium', 'Boron', 'Carbon'];
var dataObjects = [{'id': 1, 'element': {'name': 'Hydrogen'}}, {'id': 2, 'element': {'name': 'Helium'}}, {'id': 3, 'element': {'name': 'Lithium'}}, {'id': 4, 'element': {'name': 'Beryllium'}}, {'id': 5, 'element': {'name': 'Boron'}}, {'id': 6, 'element': {'name': 'Carbon'}}];

describe('FuzzySearch', function() {

    it ("Array Fuzzy Searching with IndexOf Search module (case insensitive) – perfect match", function() {
        var fuzzySearch = new FuzzySearch(dataArr, {'caseSensitive': false});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('Hydrogen');
        expect(result).to.be.eql([{"score":300,"details":[{"name":"IndexOfFS","score":100,"factor":3}],"value":"Hydrogen"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Helium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Lithium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Beryllium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Boron"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Carbon"}]);
    });

    it ("Array Fuzzy Searching with IndexOf Search module (case sensitive) – perfect match", function() {
        var fuzzySearch = new FuzzySearch(dataArr, {'caseSensitive': true});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('Hydrogen');
        expect(result).to.be.eql([{"score":300,"details":[{"name":"IndexOfFS","score":100,"factor":3}],"value":"Hydrogen"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Helium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Lithium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Beryllium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Boron"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Carbon"}]);
    });

    it ("Array Fuzzy Searching with IndexOf Search module (case sensitive) – close match", function() {
        var fuzzySearch = new FuzzySearch(dataArr, {'caseSensitive': true});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('hydrogen');
        expect(result).to.be.eql([{"score":262.5,"details":[{"name":"IndexOfFS","score":87.5,"factor":3}],"value":"Hydrogen"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Helium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Lithium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Beryllium"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Boron"},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":"Carbon"}]);
    });

    it ("Object Fuzzy Searching with IndexOf Search module (case insensitive) – perfect match", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': false, 'termPath': 'element.name'});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('Hydrogen');
        expect(result).to.be.eql([{"score":300,"details":[{"name":"IndexOfFS","score":100,"factor":3}],"value":{"id":1,"element":{"name":"Hydrogen"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":2,"element":{"name":"Helium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":3,"element":{"name":"Lithium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":4,"element":{"name":"Beryllium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":5,"element":{"name":"Boron"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":6,"element":{"name":"Carbon"}}}]);
    });

    it ("Object Fuzzy Searching with IndexOf Search module (case sensitive) – perfect match", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': true, 'termPath': 'element.name'});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('Hydrogen');
        expect(result).to.be.eql([{"score":300,"details":[{"name":"IndexOfFS","score":100,"factor":3}],"value":{"id":1,"element":{"name":"Hydrogen"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":2,"element":{"name":"Helium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":3,"element":{"name":"Lithium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":4,"element":{"name":"Beryllium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":5,"element":{"name":"Boron"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":6,"element":{"name":"Carbon"}}}]);
    });

    it ("Object Fuzzy Searching with IndexOf Search module (case sensitive) – close match", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': true, 'termPath': 'element.name'});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
        var result = fuzzySearch.search('hydrogen');
        expect(result).to.be.eql([{"score":262.5,"details":[{"name":"IndexOfFS","score":87.5,"factor":3}],"value":{"id":1,"element":{"name":"Hydrogen"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":2,"element":{"name":"Helium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":3,"element":{"name":"Lithium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":4,"element":{"name":"Beryllium"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":5,"element":{"name":"Boron"}}},{"score":0,"details":[{"name":"IndexOfFS","score":0,"factor":3}],"value":{"id":6,"element":{"name":"Carbon"}}}]);
    });

    it ("Only return results with a minimum score", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': true, 'termPath': 'element.name', 'minimumScore': 25});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 1}));
        var result = fuzzySearch.search('hydrogen');
        expect(result).to.be.eql([{"score":87.5,"details":[{"name":"IndexOfFS","score":87.5,"factor":1}],"value":{"id":1,"element":{"name":"Hydrogen"}}}]);
    });

    it ("Return empty array if option 'returnEmptyArray' is set to true", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': true, 'termPath': 'element.name', 'minimumScore': 125, 'returnEmptyArray': true});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 1}));
        var result = fuzzySearch.search('hydrogen');
        expect(result).to.be.eql([]);
    });

    it ("Return null if option 'returnEmptyArray' is set to false (default)", function() {
        var fuzzySearch = new FuzzySearch(dataObjects, {'caseSensitive': true, 'termPath': 'element.name', 'minimumScore': 125});
        fuzzySearch.addModule(IndexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 1}));
        var result = fuzzySearch.search('hydrogen');
        expect(result).to.be.eql(null);
    });

});
