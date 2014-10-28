var _           = require('lodash');
var path        = require('path');
var Benchmark   = require('benchmark');
var SearchIndex = require('../vendor/search_index');
var Reader      = require('../lib/reader');

var cities_file_path = path.join(__dirname, '..', 'data', 'cities.tsv');
var suite = new Benchmark.Suite;

// Tokenizers
function cityTokenizer(obj) {
  str = obj.name.toLowerCase();
  return str ? str.split(/\W+/) : [];
}
function queryTokenizer(obj) {
  str = obj.toString().toLowerCase();
  return str ? str.split(/\W+/) : [];
}

// For this benchmark we need actual city names to search
// on. So we will user our already written Reader and load
// the real city dataset.
var reader = new Reader();
reader.load(cities_file_path, function(cities) {

  // We needed to wait for the cities to put them in the
  // search index
  var search_index = new SearchIndex({
    datumTokenizer: cityTokenizer,
    queryTokenizer: queryTokenizer
  });
  search_index.add(cities);

  // Run the suite
  suite
    .add('RegExp search', function() {
      var found_cities = [];
      _.each(cities, function(city) {
        if (/^mon/.test(city.name)) {
          found_cities.push(city);
        }
      });
    })
    .add('IndexOf search', function() {
      var found_cities = [];
      _.each(cities, function(city) {
        if (city.name.indexOf('mon') === 0) {
          found_cities.push(city);
        }
      });
    })
    .add('Trie search', function() {
      var found_cities = search_index.get('mon');
    })

    .on('cycle', function(event) {
      console.log(String(event.target));
    })
    .on('complete', function() {
      console.log('Fastest is ' + this.filter('fastest').pluck('name'));
    })

    .run({ 'async': true });

});

// Sample benchmark
// $ node benchmarks/trie-vs-indexof-vs-regexp.js
// Finished loading cities.
// RegExp search x 2,303 ops/sec ±0.87% (95 runs sampled)
// IndexOf search x 1,790 ops/sec ±0.81% (90 runs sampled)
// Trie search x 12,931 ops/sec ±0.62% (95 runs sampled)
// Fastest is Trie search
