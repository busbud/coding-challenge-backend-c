var expect = require('chai').expect;
var app = require('../../app');
var request = require('supertest')(app);
var utilMethods = require('../../utils/util_methods');


// I'm ashamed of doing that but as long as db is not setup,
// server return a 503
var waitForServerReady = function(done) {
  var attemptDelayInMilliseconds = 100;
  request
    .get('/suggestions?q=testsetup')
    .end(function(err, res) {
      var response = res;
      if (response.statusCode !== 503) {
        console.log('Server seems up and running!');
        done();
      } else {
        setTimeout(function() {
          waitForServerReady(done);
        }, attemptDelayInMilliseconds);
      }
    });
};


describe('GET /suggestions', function() {

  before(function(done) {
    waitForServerReady(done);
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
      request
        .get('/suggestions?q=Montreal')
        .end(function(err, res) {
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
          var name = utilMethods.unidecodeString(suggestion.name);
          var patternInsensitive = /montreal/i;
          return patternInsensitive.test(name);
        });
      })
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.score;
        });
      })
    });
  });
});
