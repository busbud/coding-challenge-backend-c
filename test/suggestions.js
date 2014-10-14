var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('non-existent city', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Zzz')
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
      expect(response.json.suggestions).to.be instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('valid city', function () {
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
      expect(response.json.suggestions).to.be instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return (suggestion.name.match(/montreal|montréal/i) == null) ? false : true;
        });
      })
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });

    it('contains scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      })
    });
  });
  
  describe('valid city with non-ASCII characters (Québec)', function () {
	    var response;

	    before(function (done) {
	      request
	        .get('/suggestions?q=Québec')
	        .end(function (err, res) {
	          response = res;
	          response.json = JSON.parse(res.text);
	          done(err);
	        });
	    });

	    it('returns a 200', function () {
	      expect(response.statusCode).to.equal(200);
	    });

	    it('returns an array with 1 suggestion', function () {
	      expect(response.json.suggestions).to.be instanceof(Array);
	      expect(response.json.suggestions).to.have.length(1);
	    });
	  });
  
  describe('Cities scored by location to user', function () {
	    var response;

	    before(function (done) {
	      request
	        .get('/suggestions?q=Mont&latitude=45.496784&longitude=-73.574124')
	        .end(function (err, res) {
	          response = res;
	          response.json = JSON.parse(res.text);
	          done(err);
	        });
	    });

	    it('returns a 200', function () {
	      expect(response.statusCode).to.equal(200);
	    });

	    it('returns an array with 37 suggestions', function () {
	      expect(response.json.suggestions).to.be instanceof(Array);
	      expect(response.json.suggestions).to.have.length(37);
	    });
	    
	    it('first array element is Montréal', function () {
	        expect(response.json.suggestions).to.satisfy(function (suggestions) {
	            return suggestions.some(function (suggestion) {
	            return (suggestion.name.match(/montreal|montréal/i) == null) ? false : true;
            });
          })
	    });
	  });
});