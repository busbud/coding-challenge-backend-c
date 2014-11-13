var expect  = require('chai').expect;

var SuggestedCity = require("../src/model/SuggestedCity");

describe('SuggestedCity model', function() {
  var below5000City;
  var a5000City;
  var above5000City;
  var foreignCity;
  var frenchCharCity;
  var noXCharCity;
  var dashCharCity;

  before(function (done) {
    below5000City = new SuggestedCity(below5000PopCityOpt);
    a5000City = new SuggestedCity(a5000PopCityOpt);
    above5000City = new SuggestedCity(above5000PopCityOpt);
    foreignCity = new SuggestedCity(foreignCityOpt);
    frenchEAcuteCharCity = new SuggestedCity(frenchEAcuteCharCityOpt);
    noXCharCity = new SuggestedCity(noXCharCityOpt);
    dashCharCity = new SuggestedCity(dashCharCityOpt);
    done();
  });

  describe('fitsCriteria', function () {
    describe('validate size', function () {
      it('excludes small cities', function () {
        expect(below5000City.fitsCriteria(commonChar)).to.equal(false);
      });

      it('includes medium-large cities', function () {
        expect(a5000City.fitsCriteria(commonChar)).to.equal(true);
        expect(above5000City.fitsCriteria(commonChar)).to.equal(true);
      });
    });

    describe('validate country', function () {
      it('excludes europe cities', function () {
        expect(foreignCity.fitsCriteria(commonChar)).to.equal(false);
      });
    });

    describe('validate name', function () {
      it('excludes non-matching names', function () {
        expect(noXCharCity.fitsCriteria("x")).to.equal(false);
      });

      it('includes french-charactered city names', function () {
        expect(frenchEAcuteCharCity.fitsCriteria("é")).to.equal(true);
        expect(frenchEAcuteCharCity.fitsCriteria("e")).to.equal(true);
      });

      it('includes regular-expression-reserved-characters', function () {
        expect(dashCharCity.fitsCriteria("-")).to.equal(true);
      })
    });
  });

  describe('setScore', function () {
    it('gives 0 if no lat.', function () {
      above5000City.setScore(null, 99.10);
      expect(above5000City.output(1000, 20).score).to.equal(0);
    });

    it('gives 0 if no long.', function () {
      above5000City.setScore(-45.10, null);
      expect(above5000City.output(1000, 20).score).to.equal(0);
    });

    it('gives 0 if no lat. and long.', function () {
      above5000City.setScore(null, null);
      expect(above5000City.output(1000, 20).score).to.equal(0);
    });

    it('gives 1 if only result and exactly on it', function () {
      above5000City.setScore(above5000City.city.latitude, above5000City.city.longitude);
      expect(above5000City.output(0, 1).score).to.equal(1);
    });

    it('gives 1 if only result', function () {
      var highestScore = above5000City.setScore(above5000City.city.latitude + 1, above5000City.city.longitude + 1);
      expect(above5000City.output(highestScore, 1).score).to.equal(1);
    });

    it('gives between 0 and 1 for general case', function () {
      var cityScore = above5000City.setScore(above5000City.city.latitude + 1, above5000City.city.longitude + 1);
      var highestScore = cityScore + 40;
      var outputScore = above5000City.output(highestScore, 20).score;
      expect(outputScore).to.not.equal(1);
      expect(outputScore).to.not.equal(0);
      expect(outputScore).to.be.greaterThan(-0.1);
      expect(outputScore).to.be.lessThan(1.1);
    });
  });
});

var saintCharlesSurRichelieu = {
  name: "Saint-Charles-Sur-Richelieu",
  country: "CA",
  population: 2000,
  lat: 45.69035,
  long: -73.1850
};

var montSaintHilaire = {
  name: "Mont-Saint-Hilaire",
  country: "CA",
  population: 5000,
  lat: 45.56678,
  long: -73.19915
};

var paris = {
  name: "Paris",
  country: "FR",
  population: 300000,
  lat: 48.85025,
  long: 2.369441
};

var montreal = {
  name: "Montréal",
  country: "CA",
  population: 150000,
  lat: 45.5086699,
  long: -73.553992
};

var newark = {
  name: "Newark",
  country: "US",
  population: 150000,
  lat: 40.7127837,
  long: -74.005941
};

var vancouver = {
  name: "Vancouver",
  country: "CA",
  population: 500000,
  lat: 49.242707,
  long: -123.110904
};

var commonChar = exports.commonChar = "a";

var below5000PopCityOpt = saintCharlesSurRichelieu;
var a5000PopCityOpt = montSaintHilaire;
var above5000PopCityOpt = montreal;
var foreignCityOpt = paris;
var frenchEAcuteCharCityOpt = montreal;
var noXCharCityOpt = montreal;
var dashCharCityOpt = montSaintHilaire;

exports.validCityOpt = montreal;

exports.threeValidCityOpts = [ montreal, vancouver, newark ];
exports.threeValidCitiesQueryLat = montreal.lat;
exports.threeValidCitiesQueryLong = montreal.long;
exports.threeValidCitiesExpectedOrder = [ 0, 2, 1 ];

exports.twoInvalidCityOpts = [ paris, saintCharlesSurRichelieu ];