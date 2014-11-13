var expect  = require('chai').expect;

var SuggestedCityModelTest = require("./SuggestedCityModel.js");
var Suggestions = require("../src/model/Suggestions");

describe('Suggestions model', function() {
  var empty;
  var oneCity;
  var threeCities;
  var invalidCities;

  before(function (done) {
    empty = new Suggestions();

    var queryOptions = {
      q: SuggestedCityModelTest.commonChar,
      latitude: SuggestedCityModelTest.threeValidCitiesQueryLat,
      longitude: SuggestedCityModelTest.threeValidCitiesQueryLong
    };

    oneCity = new Suggestions();
    oneCity.add(SuggestedCityModelTest.validCityOpt, queryOptions);

    threeCities = new Suggestions();
    threeCities.add(SuggestedCityModelTest.threeValidCityOpts[0], queryOptions);
    threeCities.add(SuggestedCityModelTest.threeValidCityOpts[1], queryOptions);
    threeCities.add(SuggestedCityModelTest.threeValidCityOpts[2], queryOptions);

    invalidCities = new Suggestions();
    invalidCities.add(SuggestedCityModelTest.twoInvalidCityOpts[0], queryOptions);
    invalidCities.add(SuggestedCityModelTest.twoInvalidCityOpts[1], queryOptions);

    done();
  });

  describe('isEmpty', function () {
    it('returns true if none were added', function() {
      expect(empty.isEmpty()).to.equal(true);
    });
    it('returns true if only invalid ones were added', function() {
      expect(invalidCities.isEmpty()).to.equal(true);
    });
    it('returns false if one or more valid ones were added', function() {
      expect(oneCity.isEmpty()).to.equal(false);
      expect(threeCities.isEmpty()).to.equal(false);
    });
  });

  describe('get', function() {
    it('outputs one or more results', function() {
      expect(oneCity.get().length).to.equal(1);
      expect(threeCities.get().length).to.be.greaterThan(1);
    });
    it('sorts results in a descending order', function() {
      expect(threeCities.get()).to.satisfy(function (results) {
        // Looping to make sure everything is in the expected order.
        var unexpectedOrderPosition = false;
        var index = 0;
        results.forEach(function (r) {
          var expectedCityIndex = SuggestedCityModelTest.threeValidCitiesExpectedOrder[index];
          var expectedCity = SuggestedCityModelTest.threeValidCityOpts[expectedCityIndex];

          if (expectedCity.name !== r.name) {
            unexpectedOrderPosition = true;
          }

          index++;
        });
        return !unexpectedOrderPosition;
      });
    })
  });
});