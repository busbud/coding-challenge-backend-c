const matchers = require('jest-extended');
const startServer     = require('../server');
let request = require('supertest');
expect.extend(matchers);

describe('GET /suggestions', function() {

  beforeAll(function(done) {
    startServer(3000).then(function(app) {
      request = request(app);
      done();
    });
  })

  describe('with a non-existent city', function () {
    var response;

    beforeAll(function (done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function () {
      expect(response.statusCode).toEqual(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.json.suggestions).toMatchObject([]);
      expect(response.json.suggestions).toHaveLength(0);
    });
  });

  describe('with a valid city', function () {
    var response;

    beforeAll(function (done) {
      request
        .get('/suggestions?q=Montreal')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).toEqual(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions.length).toBeGreaterThan(0);
    });

    describe('Validate the shape of the data being returned', function() {
      it('contains latitudes and longitudes', function () {	
        const hasLongLat = (entry) => entry.latitude && entry.longitude;
        expect(response.json.suggestions).toSatisfyAll(hasLongLat)	
      });	

      it('contains scores', function () {
        const hasScore = (entry) => entry.score;
        expect(response.json.suggestions).toSatisfyAll(hasScore)	
      });
    });  

    it('contains a match', function () {
      const hasMatch = (entry) => entry.name.includes('Montréal');
      expect(response.json.suggestions).toSatisfyAny(hasMatch)
    });
  });

  describe('with invalid request format for city', function () {
    var response;

    beforeAll(function (done) {
      request
        .get('/suggestions?q=213123123')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function () {
      expect(response.statusCode).toEqual(400);
    });
  });


  describe('with invalid request format for coordinates', function () {
    var response;

    beforeAll(function (done) {
      request
        .get('/suggestions?q=montreal&longitude=Invalid')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function () {
      expect(response.statusCode).toEqual(400);
    });
  });

  describe('with mixed case', function () {
    var response;

    beforeAll(function (done) {
      request
        .get('/suggestions?q=MonTrEaL')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).toEqual(200);
    });

    it('contains a match', function () {
      const hasMatch = (entry) => entry.name.includes('Montréal');
      expect(response.json.suggestions).toSatisfyAny(hasMatch)
    });
  });

});
