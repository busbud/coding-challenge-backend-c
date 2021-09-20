var expect  = require('chai').expect;
var app     = require('../dist/app');
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('with an empty query string', function () {
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

    it('returns a 400', function () {
      expect(response.statusCode).to.equal(400);
    });
  });

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

    describe('Validate the shape of the data being returned', function() {
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
            return suggestion.score;	
          });	
        })	
      });
    });
    
    //it('is a gratuitously failing test you should remove to prove you ran the tests', function () {	
      //expect(true).to.equal(false);	
    //});	    

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          var expr = new RegExp('Montréal');
          return expr.test(suggestion.name);
        });
      })
    });

    it('returns a suggestion with a score of 1.0 on the matching item', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.filter(suggestion => suggestion.score === 1).length === 1 
      });
    });
  });

  describe('with a valid prefix', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Saska')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns 2 suggestions in descending score order', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.length === 2 && suggestions[0].score > suggestions[1].score;
      });
    });
  });

  describe('with a valid city and coordinates', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Saska&latitude=53.7&longitude=-113.2')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('check that all scores are between 0 and 1', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(suggestion => suggestion.score >= 0 && suggestion.score <= 1); 
      });
    });
  });

  describe('with a valid prefix that returns multiple, similar results', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Ab')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('check that all names are unique', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        var unique = suggestions.filter((item, index, arr) => arr.map(suggestion => suggestion.name).indexOf(item.name) === index);
        return suggestions.length === unique.length;
      });
    });
  });
});
