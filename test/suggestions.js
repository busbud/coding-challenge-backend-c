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

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
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
        .get('/suggestions?q=montreal')
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
          return suggestion.ascii.match(/montreal/i);
        });
      })
    });

    it('contains latitudes and longitudes', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.lat && suggestion.long;
        });
      })
    });

    it('is sorted by descending scores', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        var maxScore = 1.01;
        return suggestions.every(function (suggestion) {
          var decreasing = (suggestion.score <= maxScore) ? true : false;
          maxScore = suggestion.score;
          return decreasing;
        });
      })
    });
  });
  describe('with query=London and lat,long for Ohio', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=london&latitude=39.9&longitude=-83.4')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('has London,Ohio,US ranked higher than London,Kentucky,US', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      getName = function(e) {return e.fullname;}
      var ohioIndex = response.json.suggestions.map(getName).indexOf("London,Ohio,US");
      var kentuckyIndex = response.json.suggestions.map(getName).indexOf("London,Kentucky,US");
      expect(ohioIndex).to.be.below(kentuckyIndex);
    });

    it('has partial matches', function(){
      expect(response.json.suggestions).to.be.instanceof(Array);
      getName = function(e) {return e.name;}
      expect(response.json.suggestions.map(getName)).to.include('Londonderry');
    });
  });
});
