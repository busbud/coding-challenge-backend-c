const {expect}  = require('chai');
const app     = require('../app');
const request = require('supertest')(app);
const diacritics = require('diacritics');

before('wait for express to be ready', done => {
  app.on('serverReady', done);
});

describe('GET /suggestions', () => {
  describe('without providing a q parameter', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('s=Manhatt');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('when providing a too short q parameter', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('when providing a latitude without longitude', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=Mont&latitude=46.4042');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('when providing a latitude with a 0 length longitude', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=Mont&latitude=46.4042&longitude=');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('when providing a longitude without latitude', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=Mont&longitude=-72.8929');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('when providing a longitude with a 0 length latitude', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=Mont&longitude=-72.8929&latitude=');
    });

    it('returns a 400', () => {
      expect(response.statusCode).to.equal(400);
    });

    it('returns some error text', () => {
      expect(response.body.error).to.be.a('string');
    });
  });

  describe('with a non-existent city', () => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=SomeRandomCityInTheMiddleOfNowhere');
    });

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.body.suggestions).to.be.an('array');
      expect(response.body.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', ()  => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query('q=Montreal');
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.body.suggestions).to.be.an('array');
      expect(response.body.suggestions).to.have.length.above(0);
    });

    it('contains a match', () => {
      const match = response.body.suggestions.find(item => diacritics.remove(item.name).toLowerCase().includes('montreal'));
      expect(match).to.be.an('object');
    });

    it('contains latitudes and longitudes', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => suggestions.every(suggestion => suggestion.latitude && suggestion.longitude));
    });

    it('contains scores', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => suggestions.every(suggestion => typeof suggestion.score !== 'undefined'));
    });
  });

  describe('with a valid city in a foreign language', ()  => {
    let response;

    before('make call', async () => {
      response = await request.get('/suggestions').query(`q=${encodeURIComponent('Бостон')}`); // Boston in Russian or something like that
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.body.suggestions).to.be.an('array');
      expect(response.body.suggestions).to.have.length.above(0);
    });

    it('contains a match', () => {
      const match = response.body.suggestions.find(item => diacritics.remove(item.name).toLowerCase().includes('boston'));
      expect(match).to.be.an('object');
    });

    it('contains latitudes and longitudes', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => suggestions.every(suggestion => suggestion.latitude && suggestion.longitude));
    });

    it('contains scores', () => {
      expect(response.body.suggestions).to.satisfy(suggestions => suggestions.every(suggestion => typeof suggestion.score !== 'undefined'));
    });
  });
});
