var _        = require('lodash');
var expect   = require('chai').expect;
var startApp = require('../app');
var request  = require('supertest');

function startServerAndRequest(response, url) {
  return function beforeHook(done) {
    startApp(function(app) {
      request(app)
        .get(url)
        .end(function (err, res) {
          //_.assign(response, res);
          response.statusCode = res.statusCode;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });
  }
}

describe('GET /suggestions', function() {
  describe('with a non-existent city', function () {
    var response = {};

    before(startServerAndRequest(response, '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere'));

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', function () {
    var response = {};

    before(startServerAndRequest(response, '/suggestions?q=Montreal'));

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
          return /montreal/i.test(suggestion.name);
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
  });
});