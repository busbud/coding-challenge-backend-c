import { stub } from "sinon";
let expect = require("chai").expect;
let app = require("../app");
let suggestionServiceModule = require("../services/citySuggestionService");
let request = require("supertest")(app);

describe("GET /suggestions", function() {
  describe("when CitySuggestionService throws", function() {
    let response;
    let stubSuggestionService;

    before(function(done) {
      stubSuggestionService = stub(
        suggestionServiceModule.CitySuggestionService,
        "getSuggestions"
      ).throws(new Error("suggestionServiceError"));

      request
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    after(function() {
      stubSuggestionService.restore();
    });

    it("returns a 500", function() {
      expect(response.statusCode).to.equal(500);
    });

    it("returns an error object instead of suggestions", function() {
      expect(
        !response.json.suggestions &&
          response.json.message &&
          response.json.stack
      );
      expect(response.json.message).to.equal("suggestionServiceError");
    });
  });

  describe("with no query city", function() {
    let response;

    before(function(done) {
      request.get("/suggestions").end(function(err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it("returns a 400", function() {
      expect(response.statusCode).to.equal(400);
    });

    it("returns an error object instead of suggestions", function() {
      expect(!response.json.suggestions && response.json.message);
      expect(response.json.message).to.equal("No query was specified");
    });
  });

  describe("with a non-existent city", function() {
    let response;

    before(function(done) {
      request
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 404", function() {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe("with a valid city", function() {
    let response;

    before(function(done) {
      request.get("/suggestions?q=Montreal").end(function(err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it("returns a 200", function() {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    describe("Validate the shape of the data being returned", function() {
      it("contains latitudes and longitudes", function() {
        expect(response.json.suggestions).to.satisfy(function(suggestions) {
          return suggestions.every(function(suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it("contains scores", function() {
        expect(response.json.suggestions).to.satisfy(function(suggestions) {
          return suggestions.every(function(suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });
    });

    it("contains a match", function() {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          return /Montréal/.test(suggestion.name);
        });
      });
    });
  });
});
