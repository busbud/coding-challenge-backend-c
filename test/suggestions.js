let chai = require('chai');
let expect  = chai.expect;
let chaiHttp = require('chai-http');
let app = require("../app");

chai.use(chaiHttp);

describe('GET /suggestions', function() {

  describe('with a valid city', function() {
    var response;

    before((done) => {
        chai.request(app).get('/suggestions?q=Montreal').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done();
        });
    });

    it('returns a 200', function() {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', function() {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          let pattern = /montreal/i;
          return pattern.test(suggestion.name);
        });
      })
    });

    it('contains latitudes and longitudes', function() {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return (suggestion.latitude !== undefined) && (suggestion.longitude !== undefined);
        });
      })
    });

    it('contains scores', function() {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.every(function (suggestion) {
          return suggestion.score !== undefined;
        });
      })
    });
  });

    describe('with a non-existent city', function () {
        var response;

        before((done) => {
            chai.request(app).get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere').end((err, res) =>  {
                response = res;
                response.json = JSON.parse(res.text);
                done();
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
});