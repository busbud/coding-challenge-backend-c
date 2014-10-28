var Benchmark = require('benchmark');
var SplayTree = require('./splay_tree');

var AMOUNT_OF_CITIES = 10000;
var suite = new Benchmark.Suite;

// Function to generate city with some data
function newCity() {
  return {
    name: 'Montréal',
    admin1: 'QC',
    counrty: 'Canada',
    ascii: 'Montreal',
    latitude: -43.1241451,
    longitude: 71.1498712
  };
}

function randomIndex() {
  return Math.floor(Math.random() * AMOUNT_OF_CITIES) + 1;
}

// Fill in our two candidates with 100 000 cities
var cities_plain = {};
var cities_tree = new SplayTree();
for (var i = 1; i <= AMOUNT_OF_CITIES; i++) {
  cities_plain[i] = newCity();
  cities_tree.insert(i, newCity());
}

// Run the suite
suite
  .add('Array retrival', function() {
    var city = cities_plain[randomIndex()];
  })
  .add('Tree retrival', function() {
    var city = cities_tree.find(randomIndex()).value;
  })

  .on('cycle', function(event) {
    console.log(String(event.target));
  })
  .on('complete', function() {
    console.log('Fastest is ' + this.filter('fastest').pluck('name'));
  })

  .run({ 'async': true });

// Sample results
//
// Array retrival x 34,829,664 ops/sec ±1.02% (90 runs sampled)
// Tree retrival x 3,272,637 ops/sec ±0.73% (90 runs sampled)
// Fastest is Array retrival
