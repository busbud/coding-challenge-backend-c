var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);


describe('GET /suggestions', function() {
  var response;

  before(function () {
    require('../lib/store')._reset();
  });

  function doRequest (url, cb) {
    request
      .get(url)
      .end(function (err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        cb(err);
      });
  }

  describe('with no query', function () {
    before(function (done) {
      doRequest('/suggestions?not-q=Montreal', done);
    });

    it('should return a 400 error', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

  describe('with only one coordinate', function () {
    before(function (done) {
      doRequest('/suggestions?q=Montreal&latitude=10.2', done);
    });

    it('should return a 400 error', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

  describe('with one coordinate out of bound', function () {
    before(function (done) {
      doRequest('/suggestions?q=Montreal&latitude=200.2&longitude=2', done);
    });

    it('should return a 400 error', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

  describe('with a non-existent city', function () {

    before(function (done) {
      doRequest('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere', done);
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
    before(function (done) {
      doRequest('/suggestions?q=Montreal', done);
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
          // `test()` is a regExp method, not a String one...
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
          return suggestion.score;
        });
      })
    });
  });
});