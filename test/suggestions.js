const expect = require('chai').expect;
const app = require('../app');
const supertest = require('supertest');

describe('GET /suggestions', function() {

   // remark for reviewer : I added this because I modified app.js using async
  before(done => {
    app.then(s => {
      request = supertest(s);
      done();
    });
  });

  describe('with a non-existent city', function() {
    var response;

    before(function(done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function() {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', function() {
    var response;

    before(function(done) {
      request.get('/suggestions?q=Montreal').end(function(err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          // remark for reviewer : I modified "test" to "match" because "test" is a function of Regexp and "match" is a function of String
          // otherwise it would mean that suggestion.name is a regexp instance and that would be very weird for a json response ?!
          return suggestion.name.match(/montréal/i);
        });
      });
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });
});
