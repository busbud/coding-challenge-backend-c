var expect  = require('chai').expect;
var app     = require('../app');
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
          return /montréal/i.test(suggestion.name);
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
          return suggestion.score !== undefined;
        });
      })
    });
  });

  describe('with latitude and longitude', function() {
    var responseWithPosition;
    var responseNoPosition;

    before(function (done) {
      // Coordinates for Columbus, Ohio
      // Top ranked city should London, OH, USA
      request
        .get('/suggestions?q=londo&latitude=39.9833&longitude=-82.9833')
        .end(function (err, res) {
          responseWithPosition = res;
          responseWithPosition.json = JSON.parse(res.text);

          done(err);
        });
    });

    it('returns a 200', function () {
      expect(responseWithPosition.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(responseWithPosition.json.suggestions).to.be.instanceof(Array);
      expect(responseWithPosition.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function () {
      expect(responseWithPosition.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return /london/i.test(suggestion.name);
        });
      })
    });

    it('contains latitudes and longitudes', function () {
      expect(responseWithPosition.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores', function () {
      expect(responseWithPosition.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.score !== undefined;
        });
      })
    });

  })
});