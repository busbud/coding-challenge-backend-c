var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);

describe('GET /suggestions', function () {
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
          return suggestion.name.match(/montr[eé]al/i);
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
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });

  describe('with an invalid longitude', function () {
    var response;

    before(function (done) {
      request.get('/suggestions?longitude=abc').end(function (err, res) {
        response = res;
        done(err);
      });
    });

    it('returns a 400', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

  describe('with an invalid latitude', function () {
    var response;

    before(function (done) {
      request.get('/suggestions?latitude=abc').end(function (err, res) {
        response = res;
        done(err);
      });
    });

    it('returns a 400', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

  describe('with a city that appears twice in the DB', function () {
    var response;

    before(function (done) {
      request.get('/suggestions?q=monticello+ky').end(function (err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it('returns only one instance', function () {
      expect(response.json.suggestions).to.have.length(1);
    });
  });

});

describe('POST /suggestions', function () {
  var response;

  before(function (done) {
    request.post('/suggestions').end(function (err, res) {
      response = res;
      done(err);
    });
  });

  it('returns a 405', function () {
    expect(response.statusCode).to.equal(405);
  });
});
