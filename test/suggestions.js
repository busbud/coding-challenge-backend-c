var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('with a non-existent city', function () {
    var response;

    before(function (done) {
      request
        .post('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .set('content-type', 'application/json')
        .set('x-busbud-token' , 'PARTNER_JSWsVZQcS_KzxNRzGtIt1A')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
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
        .post('/suggestions?q=Montreal')
        .set('content-type', 'application/json')
        .set('x-busbud-token' , 'PARTNER_JSWsVZQcS_KzxNRzGtIt1A')
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
        var regex = /Montreal/i;
          return regex.test(suggestion.name);
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

  describe('with a wrong token', function () {
      var response;

      before(function (done) {
        request
          .post('/suggestions?q=Montreal')
          .set('content-type', 'application/json')
          .set('x-busbud-token' , 'nothing')
          .end(function (err, res) {
            response = res;
            done(err);
          });
      });

      it('returns a 500', function () {
        expect(response.statusCode).to.equal(500);
      });
    });
});