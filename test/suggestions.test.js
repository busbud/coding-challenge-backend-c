require('dotenv').config();

const expect = require('chai').expect;
const app = require('../src/app').createServer();
const request = require('supertest')(app);

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    let response;

    before(done => {
      request.get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere').end((err, res) => {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', () => {
    let response;

    before(done => {
      request.get('/suggestions?q=Montreal').end((err, res) => {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.have.length.above(0);
      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.have.length.above(0);
      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.score;
        });
      });
    });

    it('contains a match', () => {
      const removeDiacritics = str => str.normalize('NFD').replace(/[\u0300-\u036f]/g, '');

      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.some(suggestion => {
          return /montreal/gi.test(removeDiacritics(suggestion.name));
        });
      });
    });
  });
});
