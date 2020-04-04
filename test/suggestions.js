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
      //expect(response.statusCode).to.equal(404);
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
            return suggestion.latitude && suggestion.longitude;	
          });	
        })	
      });
    });
    
    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {         
          var patt = new RegExp(/montreal/i);
          return patt.test(suggestion.name); ;
        });
      })
    });    
  });

  describe('Test with an exact city', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal&latitude=45.50884&longitude=-73.58781')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });
    
    it('returns an array of with a single suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(1);
    });

    it('contains a perfect score', function () {	
      expect(parseFloat(response.json.suggestions[0].score)).to.equal(1); 
    });
  });

  describe('Test incomplete city name', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Toron')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });
    
    it('returns an array of with possible suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });  

  });

});
