const {expect} = require('chai');
const supertest = require('supertest');

const app = require('../app');


const request = supertest(app);

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    let response;

    before(done => {
      request
        .get('/suggestions?q=f')
        .end((err, res) => {
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
      request
        .get('/suggestions?q=Montreal')
        .end((err, res) => {
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

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.some(suggestion => {
          return /montréal/i.test(suggestion.name);
        });
      });
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });
});