const chai = require("chai");
const expect = chai.expect;
chai.use(require("chai-sorted"));

const app = require("../src/app");
const request = require("supertest")(app);

describe("GET /suggestions", () => {
  describe("with a non-existent city", () => {
    var response;

    before(done => {
      request
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 404", () => {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length(0);
    });
  });

  describe("with a non-existent city", () => {
    var response;

    before(done => {
      request
        .get("/suggestions?q=S")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 404", () => {
      expect(response.statusCode).to.equal(400);
    });

    it("returns an error about the min length of the query parameter", () => {
      expect(response.body.error).to.eq("Missing 'q' parameter or 'q' must be at least 2 characters long");
    });
  });

  describe("with a valid city", () => {
    var response;

    before(done => {
      request
        .get("/suggestions?q=Montreal")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 200", () => {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length.above(0);
    });

    it("contains a match", () => {
      expect(response.body.suggestions).to.satisfy(suggestions => {
        return suggestions.some(suggestion => {
          let regxp = new RegExp("montréal", "i");
          return regxp.test(suggestion.name);
        });
      });
    });

    it("contains latitudes and longitudes", () => {
      expect(response.body.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("contains scores", () => {
      expect(response.body.suggestions).to.satisfy(suggestions => {
        return suggestions.every(suggestion => {
          return suggestion.score;
        });
      });
    });

    it("returns results sorted by descending score", () => {
      expect(response.body.suggestions).to.be.sortedBy("score", { descending: true });
    });
  });

  describe("with either longitude or latitude undefined", () => {
    var response;

    before(done => {
      request
        .get("/suggestions?q=Montreal&longitude=-79.4163")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 400", () => {
      expect(response.statusCode).to.equal(400);
    });

    it("returns an error about the min length of the query parameter", () => {
      expect(response.body.error).to.eq("Missing 'longitude' or 'latitude' parameters");
    });
  });

  describe("with an incorrect radius", () => {
    var response;

    before(done => {
      request
        .get("/suggestions?q=Montreal&radius=-10")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 400", () => {
      expect(response.statusCode).to.equal(400);
    });

    it("returns an error about the min length of the query parameter", () => {
      expect(response.body.error).to.eq("Bad parameter 'radius'. Value must be a number between 1 and 1000 km.");
    });
  });
});
