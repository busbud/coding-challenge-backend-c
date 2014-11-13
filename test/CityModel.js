var expect  = require('chai').expect;

var City = require("../src/model/City");

describe('City model', function() {
  describe('compute distance', function () {
    var city;

    before(function (done) {
      city = new City({
        name: "Mont-Saint-Hilaire",
        country: "CA",
        population: 15000,
        lat: 45.56678,
        long: -73.19915
      });
      done();
    });

    it('gives low number if exactly on it', function () {
      expect(city.distanceWith(45.56678, -73.19915)).to.be.lessThan(1);
    });

    it('gives close number if Montreal', function () {
      expect(city.distanceWith(45.50884, -73.58781)).to.be.lessThan(100);
    });
  });
});