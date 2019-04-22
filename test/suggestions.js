const chai = require('chai');
var expect  = chai.expect;
var app     = require('../app');
var request = require('supertest')(app);
chai.use(require('chai-things'));

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    var response;

    before(done => {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
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

  describe('with a partial string (no lat/long)', () => {
    var response;

    before(done => {
      request
        .get('/suggestions?q=Ot')
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
    });

    it('contains multiple suggestions', () => {
      expect(response.json.suggestions).to.have.length.above(1);
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy(suggestions =>
        suggestions.every(suggestion => suggestion.latitude && suggestion.longitude)
      )
    });

    it('contains scores between 0 and 1', () => {
      expect(response.json.suggestions).to.satisfy(suggestions =>
        suggestions.every(suggestion => 
          suggestion.score != null && suggestion.score >= 0 && suggestion.score <= 1
        )
      )
    });

    it('should NOT include a distance for any suggestions', () => {
      expect(response.json.suggestions).to.have.length.at.least(1);
      expect(response.json.suggestions).to.all.not.have.property('distanceInKM');
    });
  });

  describe('with a partial string and a latitude and longitude', () => {
    var response;

    before(done => {
      request
        .get('/suggestions?q=LA&latitude=45.5&longitude=-73.5')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('should include a distance with each suggestion', () => {
      expect(response.json.suggestions).to.have.length.at.least(1);
      expect(response.json.suggestions).to.all.have.property('distanceInKM');
    });

    it('should return local results reasonably high in the rankings', () => {
      expect(response.json.suggestions.slice(0, 5)).to.satisfy(suggestions =>
        suggestions.some(suggestion => suggestion.name == 'Laval')
      )
    });

    it('should return balance top population suggestions with local results', () => {
      expect(response.json.suggestions.slice(0, 5)).to.satisfy(suggestions =>
        suggestions.some(suggestion => suggestion.name == 'Las Vegas')
      )
    });
  });

  describe('with a valid and unique city', () => {
    var response;

    before(done => {
      request
        .get('/suggestions?q=Chambly')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('contains exactly one match', () => {
      expect(response.json.suggestions.length).to.equal(1);
      expect(response.json.suggestions[0].name.match(/Chambly/i));
    });
  });
});