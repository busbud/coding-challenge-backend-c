var expect  = require('chai').expect;
var app     = require('../app').app;
var request = require('supertest')(app);

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

  describe('with a valid city', function () {
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

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return suggestion.name.indexOf('Montréal') > -1;
        });
      });
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it('contains scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.score;
        });
      });
    });
    
    it('has scores in the correct (descending) order', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        var current_score = 1; // scores by definition are between 0-1, start with 1
        return suggestions.length > 1 && suggestions.every(function (suggestion) {
          var less_than = suggestion.score <= current_score;
          current_score = suggestion.score;
          return less_than;
        });
      });
    });
    
    it('contains city name, administration code and country code in the name', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.name.split(',').length === 3 && // there should be 3 'sections' in each name
            suggestion.name.split(',').every(function(section) {
              return section.length > 0; // each section should not be empty
        });
        });
      });
    });
  });
  
  describe('with non-numerical latitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=Hello&longitude=1')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });
  
  describe('with non-numerical longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=1&longitude=FooBar')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });
  
  describe('with both non-numerical latitude and longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=foo&longitude=bar')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });
  
  describe('with numerical, invalid latitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=91&longitude=45')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });
  
  describe('with numerical, invalid longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=90&longitude=181')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });
  
  describe('with numerical, invalid latitude and longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=91&longitude=181')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Latitude and Longitude must be valid numbers or both not provided');
    });
  });  
  
  describe('with both valid, numerical, latitude and longitude', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=London&latitude=-20&longitude=40')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200 and a proper suggestion', function () {
      expect(response.statusCode).to.equal(200);
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return !!suggestion.name && !!suggestion.latitude && 
            !!suggestion.longitude && !!suggestion.score;
        });
      });
    });
    
    it('can suggest an inexact match', function () {
      expect(response.statusCode).to.equal(200);
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          return suggestion.name.length > 'London'.length;
        });
      });
    });

    it('can suggest an exact match', function () {
      expect(response.statusCode).to.equal(200);
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          return suggestion.name.split(',')[0] === 'London';
        });
      });
    });  
  });
  
  describe('with no query params', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Must provide a City to your query (q)');
    });
  });
  
  describe('with only latitude and longitude (no q)', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?latitude=10&longitude=10')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400 and proper error', function () {
      expect(response.statusCode).to.equal(400);
      expect(response.json.error).to.equal('Must provide a City to your query (q)');
    });
  });
  
});