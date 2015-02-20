var expect  = require('chai').expect;
var app     = require('../app');
var request = require('supertest')(app);
var _ = require('underscore-node');

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
          return /montreal/i.test(suggestion.name);
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
          return suggestion.score;
        });
      })
    });

    it('contains scores that are between 0 and 1', function () {
      var score = response.json.suggestions[0].score;
      expect((score <= 1 && score > 0)).to.equal(true);
    });

    it('returns an array of sorted suggestions', function () {
      expect(_.every(_.first(response.json.suggestions,5), function(value, index, array) {
        if(index > 0){
          return (array[index - 1].score >= value.score);
        }else{
          return true;
        }
      })).to.equal(true);
    });

    it('only Canadian and US cities', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return !(/France/i.test(suggestion.name));
        });
      })
    });

  });

  describe('with a partial searching name', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Londo')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('contains a name which can be used to disambiguate between similarly named locations', function () {
      expect(response.json.suggestions[0].name).to.not.equal(response.json.suggestions[1].name);
    });

    it('only Canadian and US cities', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return !(/India/i.test(suggestion.name));
        });
      })
    });

  });

  describe('with latitude and longitude details', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a closer city to this point', function () {
      expect(response.json.suggestions[0].name).to.equal("London, Ontario, Canada");
    });


  });

});