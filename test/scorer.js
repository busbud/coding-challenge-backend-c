var assert  = require('chai').assert;
var scorer     = require('../lib/scorer');

describe('scorer', function() {
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

  describe('getScore with non matching string', function () {
    var geoData = {
      latitude : null,
      longitude : null
    };
    it('score should be equal to 0', function() {
      // Test implementation goes here
      var arr = [];
      var score = scorer.getScore(cityMontreal,'toto',geoData)
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
      score = scorer.getScore(cityMontreal,'Mont',geoData);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });

    it('simplify search string (M) score should be smaller', function() {;
      var newScore = scorer.getScore(cityMontreal,'M',geoData);
      assert.operator(newScore, '<', score);
      assert.operator(newScore, '>', 0);
      assert.operator(newScore, '<', 1);
    });

    it('refine search string (Montreal) score should be higher', function() {;
      var newScore = scorer.getScore(cityMontreal,'Montrea',geoData);
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
      score = scorer.getScore(cityMontreal,'Montr',geoData);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });

    it('check score with invalid geo data', function() {
      geoData = {
        latitude : 'toto',
        longitude :'toto'
      };
      var newScore = scorer.getScore(cityMontreal,'Montr',geoData);
      assert.equal(newScore, score,'score is not affected because invalid geo data');
    });

    it('check score with geo data that apply no score penalty', function() {
      geoData = {
        latitude : 45.50884,
        longitude : -73.58781
      };
      var newScore = scorer.getScore(cityMontreal,'Montr',geoData);
      assert.equal(score, newScore);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });
    it('check score with geo data that apply score penalty', function() {
      geoData = {
        latitude : 42.35843,
        longitude :-71.05977
      };
      var newScore = scorer.getScore(cityMontreal,'Montr',geoData);
      assert.notEqual(score, newScore);
      assert.operator(score, '>', newScore);
      assert.operator(score, '>', 0);
      assert.operator(score, '<', 1);
    });
  });

});
