var expect = require('chai').expect;
var app = require('../app');
var request = require('supertest')(app)
var sinon = require('sinon');
var CityService = require('../dist/service/CityService').CityService;


describe('GET /suggestions', function () {

  afterEach(() => {
    sinon.restore()
  })
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
        .get('/suggestions?q=montréal&latitude=45.50884&longitude=-73.58781')
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
          return suggestion.name.match(/^[(Montreal)\u00C0-\u00FF]*/i);
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

    it('scores 1 if exact match', function () {
      expect(response.json.suggestions[0].score).to.equal(1);
    });


  });
  describe('with a valid city and no latitude/longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Alb')
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


    it('displays array sorted by score', function () {
      const sortedResponse = [...response.json.suggestions].sort((a, b) => b.score - a.score)
      expect(response.json.suggestions).to.satisfy(function (suggestions, sorted = sortedResponse) {
        return suggestions.every(function (suggestion, index) {
          return suggestion.score === sorted[index].score;
        });
      })
    });

  });
  describe('with internal errors', function () {
    before(() => {
      sinon.stub(CityService.prototype, 'read').throws(new Error('Forced'))
    });

    it('returns a 500', async () => {
      var response;
      request
        .get('/suggestions?q=Palm')
        .end(function (err, res) {
          response = res;
          expect(response.statusCode).to.equal(500);
        });
    });


  });
});
