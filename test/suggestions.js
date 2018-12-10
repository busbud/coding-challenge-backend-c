var expect  = require('chai').expect;
var App     = require('../code/app');
var app = new App().start();
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('with a non-existent city', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return (/montréal/i).test(suggestion.name);
        });
      })
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    describe('with latitude and longitude parameters', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=London&latitude=43.70011&longitude=-79.4163&dsw=0.5')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('returns the closest location when the latitude and longitude are provided', function () {
        expect(response.json.suggestions[0].latitude).to.equal('42.98339');
      });

    });


    describe('with m maximum suggestions parameter', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=London&m=10')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('only m suggestions when the parameter is provided', function () {
        expect(response.json.suggestions.length).to.equal(10);
      });

    });

  });
});
