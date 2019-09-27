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

  describe('with an empty search query', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=')
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
          return (/montreal/i).test(suggestion.name);
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
    it('scores are in descending order', function() {
      const suggestions = response.json.suggestions;
      expect(suggestions.length).to.equal(2);
      expect(suggestions[0].score).to.be.not.below(suggestions[1].score);
    });
  });

  describe('with a valid city provided coordinates', function () {
    var response;
    before(function (done) {
      request
        .get('/suggestions?q=Montreal&longitude=-73.64918&latitude=45.45286')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('scores are affected by longitude and latitude', function() {
      const suggestions = response.json.suggestions;
      expect(suggestions.length).to.equal(2);
      expect(suggestions[0].name).to.equal("Montreal West, CA");
      expect(suggestions[1].name).to.equal("Montreal, CA");
    });
  });

  describe('with a city name that belongs to multiple cities', function () {
    var response;
    before(function (done) {
      request
        .get('/suggestions?q=Vancouver')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('name is unique despite exact city name', function() {
      const suggestions = response.json.suggestions;
      expect(suggestions.length).to.equal(2);
      expect(suggestions[0].name).to.equal("Vancouver, CA");
      expect(suggestions[1].name).to.equal("Vancouver, WA, US");
    });
  });

  describe('with an invalid longitude/latitude', function () {
    var response;
    before(function (done) {
      request
        .get('/suggestions?q=Vancouver&latitude=-100&longitude=170')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('request with invalid long/lat returns same result as no long/lat provided', function() {
      const suggestions = response.json.suggestions;
      expect(suggestions[0].score).to.equal(1);
    });
  });
});
