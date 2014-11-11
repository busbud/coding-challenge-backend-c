var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('with a missing query string', function() {
    var response;

    before(function(done) {
      request
        .get('/suggestions?q=')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('gives an error message', function() {
      expect(response.json.suggestions.error).to.equal('query parameter missing')
    });
  });

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
          return /montreal/i.test(suggestion.name);
        });
      });
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.score;
        });
      });
    });

    context('with coordinates in the query', function() {
      var response;

      context('when searching for a nearby city', function() {
        before(function (done) {
          request
            .get('/suggestions?q=Montreal&latitude=45.56995&longitude=-73.692')
            .end(function (err, res) {
              response = res;
              response.json = JSON.parse(res.text);
              done(err);
            });
        });

        it('gives the nearby city the highest score', function() {
          expect(response.json.suggestions).to.satisfy(function(suggestions) {
            return suggestions[0].score > suggestions[1].score;
          });
        });
      });
    });
  });

});
