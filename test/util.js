//var expect  = require('chai').expect;
var assert  = require('chai').assert;
var util     = require('../lib/util');

describe('util', function() {
  var cityMontreal = {
    id : 1,
    name : 'Montréal',
    asciiName :'Montreal',
    state: 'QC',
    latitude : 45.50884,
    longitude : -73.58781,
    country : 'CA',
    population : 100000,
  };

  var cityBoston = {
    id : 2,
    name : 'Boston',
    asciiName :'Boston',
    state: 'MA',
    latitude : 42.35843,
    longitude : -71.05977,
    country : 'US',
    population : 100000,
  };

  describe('formatCityName ', function () {

    it('format US City Name', function() {
      var name = util.formatCityName(cityBoston);
      assert.equal(name, 'Boston, MA, USA');
    });

    it('format Canadian City Name', function() {
      var name = util.formatCityName(cityMontreal);
      assert.equal(name, 'Montreal, QC, Canada');
    });
  });

  describe('getCanadianStateCode ', function () {

    it('get canadian state iso code with valid value', function() {
      var state = util.getCanadianStateCode(1);
      assert.equal(state, 'AB');
    });

    it('get canadian state iso code with invalid value', function() {
      var state = util.getCanadianStateCode(100);
      assert.isUndefined(state, 'no state defined for code 100')
    });
  });

  describe('sortByScore ', function () {

    var suggestions = [];
    var city1 ={name:'city1',score: 0.4};
    var city2 ={name:'city2',score: 0.3};
    var city3 ={name:'city3',score: 0.9};
    suggestions[0] = city1;
    suggestions[1] = city2;
    suggestions[2] = city3;

    it('check suggestions are sorted by descending score', function() {
      var result = util.sortByScore(suggestions);
      assert.lengthOf(result, 3);
      assert.equal(result[0], city3);
      assert.equal(result[1], city1);
      assert.equal(result[2], city2);
    });
  });

  describe('getScore with non matching string', function () {
    var geoData = {
      latitude : null,
      longitude : null
    };
    it('score should be equal to 0', function() {
      // Test implementation goes here
      var arr = [];
      var score = util.getScore(cityMontreal,'toto',geoData)
      assert.equal(score, 0)
    });
  });

  describe('getScore with matching string', function () {
    var geoData = {
      latitude : null,
      longitude : null
    };

    var score = 0;
    it('string (Montr) score should be above 0', function() {
      score = util.getScore(cityMontreal,'Mont',geoData);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });

    it('simplify search string (M) score should be smaller', function() {;
      var newScore = util.getScore(cityMontreal,'M',geoData);
      assert.operator(newScore, '<', score);
      assert.operator(newScore, '>', 0);
      assert.operator(newScore, '<', 1);
    });

    it('refine search string (Montreal) score should be higher', function() {;
      var newScore = util.getScore(cityMontreal,'Montrea',geoData);
      assert.operator(newScore, '>', score);
      assert.operator(newScore, '>', 0);
      assert.operator(newScore, '<', 1);
    });
  });

  describe('getScore with matching and string geo data' , function () {
    var geoData = {
      latitude : null,
      longitude : null
    };

    var score = 0;
    it('check score with no geo data provided ', function() {
      score = util.getScore(cityMontreal,'Montr',geoData);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });

    it('check score with invalid geo data', function() {
      geoData = {
        latitude : 'toto',
        longitude :'toto'
      };
      var newScore = util.getScore(cityMontreal,'Montr',geoData);
      assert.equal(newScore, score,'score is not affected because invalid geo data');
    });

    it('check score with geo data that apply no score penalty', function() {
      geoData = {
        latitude : 45.50884,
        longitude : -73.58781
      };
      var newScore = util.getScore(cityMontreal,'Montr',geoData);
      assert.equal(score, newScore);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });
    it('check score with geo data that apply score penalty', function() {
      geoData = {
        latitude : 42.35843,
        longitude :-71.05977
      };
      var newScore = util.getScore(cityMontreal,'Montr',geoData);
      assert.notEqual(score, newScore);
      assert.operator(score, '>', newScore);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });
  });


});
