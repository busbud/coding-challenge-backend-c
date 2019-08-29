// Following is for busbud-lint who want me to remove space between async ()
/* eslint-disable space-before-function-paren */

const expect = require('chai').expect;
const app = require('../app');
const request = require('supertest')(app);

describe('GET /suggestions', () => {
  after(async () => new Promise(resolve => app.close(resolve)));

  describe('with a non-existent city', () => {
    let response;

    before(async () => {
      response = await request.get(
        '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere'
      );
    });

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', () => {
    let response;

    before(async () => {
      response = await request.get('/suggestions?q=Montreal');
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length.above(0);
    });

    it('contains a match', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => {
        return suggestions.some(suggestion =>
          /montréal/i.test(suggestion.name)
        );
      });
    });

    it('contains latitudes and longitudes', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => {
        return suggestions.every(
          suggestion => suggestion.latitude && suggestion.longitude
        );
      });
    });

    it('contains scores', () => {
      expect(response.body.suggestions).to.satisfy(suggestions =>
        suggestions.every(
          suggestion => suggestion.score >= 0 && suggestion.score <= 1
        )
      );
    });
  });

  describe('geo biasing', () => {
    // prevent rate limiting
    afterEach(done => setTimeout(done, 500));

    it('return Washington from Utah while calling from San Francisco', async () => {
      // San Francisco coordinates
      const { body } = await request.get(
        '/suggestions?q=Washing&latitude=37.7577627&longitude=-122.4727052'
      );
      expect(body.suggestions[0].name).to.contains('Utah, US');
    });

    it('return Washington from D.C. while calling from New York', async () => {
      // San Francisco coordinates
      const { body } = await request.get(
        '/suggestions?q=Washing&latitude=40.6976633&longitude=-74.1201063'
      );
      expect(body.suggestions[0].name).to.contains('New Jersey, US');
    });
  });

  describe('allow localized names', () => {
    // prevent rate limiting
    afterEach(done => setTimeout(done, 500));

    // IT SEEMS LIKE A BUG IN SUPERAGENT - it doesn't encode query string

    it('return Washington for Вашинг', async () => {
      // San Francisco coordinates
      const { body } = await request
        .get('/suggestions')
        .query(`q=${encodeURIComponent('Вашинг')}`);
      expect(body.suggestions[0].name).to.contains('Washington');
    });

    it('return Montreal for Monreāl', async () => {
      // San Francisco coordinates
      const { body } = await request
        .get('/suggestions')
        .query(`q=${encodeURIComponent('monreāl')}`);
      expect(body.suggestions[0].name).to.contains('Montréal');
    });
  });

  describe('allow some misspellings and case insensitive names', () => {
    // prevent rate limiting
    afterEach(done => setTimeout(done, 500));

    // IT SEEMS LIKE A BUG IN SUPERAGENT - it doesn't encode query string

    it('return Montréal for Nonreal', async () => {
      // San Francisco coordinates
      const { body } = await request.get('/suggestions').query(`q=Nonreal`);
      expect(body.suggestions[0].name).to.contains('Montréal');
    });

    it('return Montreal for MONtREaL', async () => {
      // San Francisco coordinates
      const { body } = await request.get('/suggestions').query(`q=MONtREaL`);
      expect(body.suggestions[0].name).to.contains('Montréal');
    });
  });
});
