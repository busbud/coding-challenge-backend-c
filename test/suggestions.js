const expect = require('chai').expect;
var app = require('../app');
var request = require('supertest')(app);

before('wait for express application to be ready', done => {
  app.on('appReady', done);
});

describe('GET /suggestions', function() {
  describe('with a non-existent city', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function() {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('without a search term', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an error message', function() {
      expect(response.json.error).to.be.eql('Invalid query parameter');
    });
  });

  describe('without a proper latitude', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?q=Lon&longitude=-79.4163')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an error message', function() {
      expect(response.json.error).to.be.eql('Must provide a latitude');
    });
  });

  describe('without a proper longitude', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?q=Lon&latitude=43.70011')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an error message', function() {
      expect(response.json.error).to.be.eql('Must provide a longitude');
    });
  });

  describe('with invalid longitude', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?q=Lon&latitude=43.70011&longitude=-279.4163')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an error message', function() {
      expect(response.json.error).to.be.eql('Invalid longitude');
    });
  });

  describe('with invalid latitude', function() {
    var response;
    before(function(done) {
      request
        .get('/suggestions?q=Lon&latitude=143.70011&longitude=-179.4163')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function() {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an error message', function() {
      expect(response.json.error).to.be.eql('Invalid latitude');
    });
  });

  describe('with a valid city', function() {
    var response;

    before(function(done) {
      request
        .get('/suggestions?q=Montréal')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          console.log(JSON.parse(res.text));
          done(err);
        });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an error message', function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return /Montréal/i.test(suggestion.name);
        });
      });
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });

  describe('with a valid city and coordinates', function() {
    var response;

    before(function(done) {
      request
        .get('/suggestions?q=Montréal&latitude=45.45286&longitude=-73.64918')
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          console.log(JSON.parse(res.text));
          done(err);
        });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an error message', function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return /Montréal/i.test(suggestion.name);
        });
      });
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });
});
