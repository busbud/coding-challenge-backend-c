var expect  = require('chai').expect;
var app     = require('../server');
var request = require('supertest')(app);

describe('GET /suggestions', function() {
  describe('with bad query parameters', function () {
    describe('without parameters', function () {
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

    describe('with empty q parameter', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=')
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

    describe('with latitude but no longitude', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&latitude=32.2217')
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

    describe('with longitude but no latitude', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&longitude=-110.9265')
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

    describe('with latitude too large', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&latitude=90.0001&longitude=-110.9265')
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

    describe('with latitude too small', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&latitude=-90.0001&longitude=-110.9265')
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

    describe('with longitude too large', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&latitude=32.2217&longitude=180.0001')
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

    describe('with longitude too small', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Tuc&latitude=32.2217&longitude=-180.0001')
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
    describe('name query', function () {
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
    });

    describe('name and coordinates query', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Montre&latitude=45.5017&longitude=-73.5673')
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
    });
  });

  describe('with non-ascii chars', function () {
    var response;

    before(function (done) {
        request
          .get('/suggestions?q=Sainte-Thérè')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
    });

    it('returns a suggestions', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.length.above(0);
    });

  });

  describe('without regard to chars capitalization ', function () {
    var response;

    before(function (done) {
        request
          .get('/suggestions?q=mOnT-TREM')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

    it('returns a suggestions', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.length.above(0);
    });
  });

  describe('for a single match', function () {
    describe('name query', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Mississauga')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('returns a single suggestion', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.lengthOf(1)
      });

      it('contains the perfect score', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.score === 1;
          });
        })
      });
    });
    describe('name and coordinates query', function () {
      var response;

      before(function (done) {
        request
          .get('/suggestions?q=Mississauga&latitude=45.4215&longitude=-75.6972')
          .end(function (err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('returns a single suggestion', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.lengthOf(1)
      });

      it('contains the perfect score', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.score === 1;
          });
        })
      });
    });
  });
  describe('with equally valid scores', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Otta')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns equal scores', function () {
        expect(response.json.suggestions).to.be.instanceof(Array);
        expect(response.json.suggestions).to.have.lengthOf(3);
        expect(response.json.suggestions[0].score)
          .to.equal(response.json.suggestions[1].score)
          .to.equal(response.json.suggestions[2].score);
    });
  });

  describe('with known ordered scores', function () {
    var response;

    before(function (done) {
      request
        .get('/suggestions?q=Vanco&latitude=43.70011&longitude=-79.4163')
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns properly ordered scores', function () {
      var higherScore = 1;
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
          return suggestions.every(function (suggestion) {
            var isEqLower = suggestion.score <= higherScore;
            higherScore = suggestion.score;
            return isEqLower;
          });
      });
    });
  });
});