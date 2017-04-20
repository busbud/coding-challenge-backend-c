var expect    = require('chai').expect;
var supertest = require('supertest');
var utils     = require('../lib/utils');

var server = supertest.agent("http://localhost:2345");

describe('GET /suggestions', function() {

  describe('with a non-existent city', function () {
    var response;

    before(function (done) {
      server
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
      server
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
          return suggestion.score;
        });
      })
    });
  });
});


describe('utils', function() {

  var tor = {
    id      : 951321,
    name    : 'Toronto',
    ascii   : 'Toronto',
    lat     : 43.653109,
    long    : -79.388364,
    country   : 'CA',
    admin1    : '8'
  };

  var nyc = {
    id      : 753321,
    name    : 'New York City',
    ascii   : 'New York City',
    lat     : 40.712332,
    long    : -73.996550,
    country   : 'US',
    admin1    : 'NY'
  };

  var yor = {
    id      : 654123,
    name    : 'York',
    ascii   : 'York',
    lat     : 39.962398, 
    long    : -76.723152,
    country   : 'US',
    admin1    : 'PA'
  };

  describe('Score city', function () {

      describe('without location', function() {
          var query = {
              q: 'york',
              lat: null,
              lon: null
          }
          it('matching score is in [0, 1]', function () {
              utils.scoreCity(tor, query);
              utils.scoreCity(nyc, query);
              utils.scoreCity(yor, query);
              expect(tor.score).to.equal(0);
              expect(nyc.score).to.be.above(0);
              expect(yor.score).to.be.above(0);
              expect(nyc.score).to.be.below(1);
              expect(yor.score).to.be.below(1);
          });
      });


      describe('with location', function() {
          var query = {
              q: 'york',
              lat: '40.712332',
              lon: '-73.996550'
          }
          it('matching score is in [0, 1]', function () {
              utils.scoreCity(tor, query);
              utils.scoreCity(nyc, query);
              utils.scoreCity(yor, query);
              expect(tor.score).to.equal(0);
              expect(nyc.score).to.be.above(0);
              expect(yor.score).to.be.above(0);
              expect(nyc.score).to.be.below(1);
              expect(yor.score).to.be.below(1);
          });
      });

  });

  
});
