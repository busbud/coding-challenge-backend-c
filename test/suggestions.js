var expect = require("chai").expect;
var app = require("../app");
var request = require("supertest")(app);

describe("GET /suggestions", function() {
  describe("with a non-existent city", function() {
    var response;

    before(function(done) {
      request
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 404", function() {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", function() {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length(0);
    });
  });

  describe("with a valid city", function() {
    var response;

    before(function(done) {
      request
        .get("/suggestions?q=Montreal")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 200", function() {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", function() {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length.above(0);
    });

    it("contains a match", function() {
      expect(response.body.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          let regxp = new RegExp("montreal", "i");
          return regxp.test(suggestion.name);
        });
      });
    });

    it("contains latitudes and longitudes", function() {
      expect(response.body.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("contains scores", function() {
      expect(response.body.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });
  });

  describe("with either longitude or latitude undefined", function() {
    var response;

    before(function(done) {
      request
        .get("/suggestions?q=Montreal&longitude=-79.4163")
        .set("Accept", "application/json")
        .end(function(err, res) {
          if (err) return done(err);
          response = res;
          done();
        });
    });

    it("returns a 400", function() {
      expect(response.statusCode).to.equal(400);
    });
  });
});
