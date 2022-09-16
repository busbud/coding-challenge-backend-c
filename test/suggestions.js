var chai = require("chai");
var chaiHttp = require("chai-http");
var app = require("../app");

chai.should();
chai.use(chaiHttp);

const request = chai.request;

describe("GET", () => {
  var response;
  it("returns welcome message", (done) => {
    request(app)
      .get("/")
      .end((err, res) => {
        response = res;
        response.json = JSON.parse(res.text);
        response.should.have.status(200);
        response.json.should.equal("Busbud Coding Challenge");
        done();
      });
  });
});

describe("GET /suggestions", () => {
  describe("with a non-existent city", () => {
    var response;

    before((done) => {
      request(app)
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns an empty array of suggestions", () => {
      response.json.suggestions.should.be.instanceof(Array);
      response.json.suggestions.should.have.length(0);
    });
  });

  describe("with a valid city", () => {
    var response;

    before((done) => {
      request(app)
        .get("/suggestions?q=Montreal")
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 200", () => {
      response.statusCode.should.equal(200);
    });

    it("returns an array of suggestions", () => {
      response.json.suggestions.should.be.instanceof(Array);
      response.json.suggestions.should.have.length.above(0);
    });

    it("contains latitudes and longitudes", () => {
      response.json.suggestions.should.satisfy((suggestions) => {
        return suggestions.every((suggestion) => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("contains scores", () => {
      response.json.suggestions.should.satisfy((suggestions) => {
        return suggestions.every((suggestion) => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("contains a match", () => {
      response.json.suggestions.should.satisfy((suggestions) => {
        return suggestions.some((suggestion) => {
          const pattern = /montreal/i;
          return pattern.test(suggestion.name);
        });
      });
    });

    describe.skip("skipped it instead of removing", () => {
      it("is a gratuitously failing test you should remove to prove you ran the tests", function () {
        chai.expect(true).should.equal(false);
      });
    });
  });
});
