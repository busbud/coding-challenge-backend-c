import chai from 'chai';
import app from '../src/index.js';
import request from 'supertest';

describe('GET /suggestions', function () {
  describe('with a non-existent city', function () {
    var response: any;

    before(function (done) {
      request(app).get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function () {
      chai.expect(response.status).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      chai.expect(response.json).to.be.instanceof(Array);
      chai.expect(response.json).to.have.length(0);
    });
  });

  describe('with a valid city', function () {
    var response: any;

    before(function (done) {
      request(app).get('/suggestions?q=Montreal')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      chai.expect(response.status).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      chai.expect(response.json).to.be.instanceof(Array);
      chai.expect(response.json).to.have.length.above(0);
    });

    describe('Validate the shape of the data being returned', function () {
      it('contains latitudes and longitudes', function () {
        chai.expect(response.json).to.satisfy(function (suggestions: CitySuggestion[]) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('contains scores', function () {
        chai.expect(response.json).to.satisfy(function (suggestions: CitySuggestion[]) {
          return suggestions.every(function (suggestion: CitySuggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });
    });

    it('contains a match', function () {
      chai.expect(response.json).to.satisfy(function (suggestions: CitySuggestion[]) {
        return suggestions.some(function (suggestion: CitySuggestion) {
          return suggestion.name.match(/montreal/i);
        });
      })
    });
  });
});
