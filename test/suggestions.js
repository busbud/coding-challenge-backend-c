var expect = require('chai').expect;
var app = require('../app');
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

    it('should returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('should returns an empty array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('should with a valid city', function () {
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

    it('should returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('should returns an array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    describe.skip('Validate the shape of the data being returned', function () {
      it('should contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('should contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });
    });

    it('should contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return new RegExp(suggestion.name, 'i').test('montreal');
        });
      })
    });
  });
});
