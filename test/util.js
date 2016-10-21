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

});
