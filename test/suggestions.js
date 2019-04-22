const chai = require('chai');
var expect  = chai.expect;
var app     = require('../app');
var request = require('supertest')(app);
chai.use(require('chai-things'));

describe('GET /suggestions', function() {
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

  describe('with a partial string (no lat/long)', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Ot')
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
    });

    it('contains multiple suggestions', function () {
      expect(response.json.suggestions).to.have.length.above(1);
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores between 0 and 1', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          const inRange = suggestion.score >= 0 && suggestion.score <= 1;
          return suggestion.score != null && inRange;
        });
      })
    });

    it('should NOT include a distance for any suggestions', function () {
      expect(response.json.suggestions).to.have.length.at.least(1);
      expect(response.json.suggestions).to.all.not.have.property('distanceInKM');
    });
  });

  describe('with a partial string and a latitude and longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=LA&latitude=45.5&longitude=-73.5')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('should include a distance with each suggestion', function () {
      expect(response.json.suggestions).to.have.length.at.least(1);
      expect(response.json.suggestions).to.all.have.property('distanceInKM');
    });

    it('should return local results reasonably high in the rankings', function () {
      expect(response.json.suggestions.slice(0, 5)).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return suggestion.name == 'Laval';
        });
      })
    });

    it('should return balance top population suggestions with local results', function () {
      expect(response.json.suggestions.slice(0, 5)).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return suggestion.name == 'Las Vegas';
        });
      })
    });
  });

  describe('with a valid and unique city', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Chambly')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('contains exactly one match', function () {
      expect(response.json.suggestions.length).to.equal(1);
      expect(response.json.suggestions[0].name.match(/Chambly/i));
    });
  });
});