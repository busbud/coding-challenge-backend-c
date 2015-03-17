var expect  = require('chai').expect;
var app     = require('../app');
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
	    var regex = /montreal/i;
          return regex.test(suggestion.name);
        });
      })
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.loc.coordinates[0] && suggestion.loc.coordinates[1];
        });
      })
    });

    it('contains scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.score;
        });
      })
    });
  });

    describe('Limit to 20 by default with a lot of matches', function () {
	var response;
	before(function (done) {
	    request
		.get('/suggestions?q=P')
		.end(function (err, res) {
		    response = res;
		    response.json = JSON.parse(res.text);
		    done(err);
		});
	});

	it('limits the response to the 20 first', function () {
	    expect(response.json.suggestions).to.have.length.below(21);
	
	});

    });

     describe('Limit to the number requested in queryString', function () {
	var response;
	before(function (done) {
	    request
		.get('/suggestions?q=P&limit=25')
		.end(function (err, res) {
		    response = res;
		    response.json = JSON.parse(res.text);
		    done(err);
		});
	});

	it('limits the response to the 25 when limit is put in query', function () {
	    expect(response.json.suggestions).to.have.length.below(26);
	
	});

    });

     describe('with valid coordinates only', function () {
	var response;
	before(function (done) {
	    request
		.get('/suggestions?longitude=-114.35255&latitude=62.456')
		.end(function (err, res) {
		    response = res;
		    response.json = JSON.parse(res.text);
		    done(err);
		});
	});

	 it('contains a match (Yellowknife)', function () {
	     expect(response.json.suggestions).to.satisfy(function (suggestions) {
		 return suggestions.some(function (suggestion) {
		     var regex = /yellowknife/i;
		     return regex.test(suggestion.name);
		 });
	     })
	 });

    });

    describe('with valid coordinates and valid partial name', function () {
	var response;
	before(function (done) {
	    request
		.get('/suggestions?q=Yello&longitude=-114.35255&latitude=62.456')
		.end(function (err, res) {
		    response = res;
		    response.json = JSON.parse(res.text);
		    done(err);
		});
	});

	 it('contains a match (Yellowknife)', function () {
	     expect(response.json.suggestions).to.satisfy(function (suggestions) {
		 return suggestions.some(function (suggestion) {
		     var regex = /yellowknife/i;
		     return regex.test(suggestion.name);
		 });
	     })
	 });

    });
    
    describe('with a empty query name parameter', function () {
	var response;

	before(function (done) {
	    request
		.get('/suggestions?q=')
		.end(function (err, res) {
		    response = res;
		    response.text = res.text;
		    done(err);
		});
	});

	it('returns a 400', function () {
	    expect(response.statusCode).to.equal(400);
	});
    });

});
