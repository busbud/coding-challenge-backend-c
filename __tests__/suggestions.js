const app = require("../app").app;
const request = require("supertest")(app);

describe("GET /suggestions", function() {
  describe("with a non-existent city", function() {
    let response;

    beforeAll(function(done) {
      request
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end(function(err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 404", function() {
      expect(response.statusCode).toEqual(404);
    });

    it("returns an empty array of suggestions", function() {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions).toHaveLength(0);
    });
  });

  describe("with a valid city", function() {
    var response;

    beforeAll(function(done) {
      request.get("/suggestions?q=Montreal").end(function(err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it("returns a 200", function() {
      expect(response.statusCode).toEqual(200);
    });

    it("returns an array of suggestions", function() {
      expect(response.json.suggestions).toBeInstanceof(Array);
      expect(response.json.suggestions.length).toBeGreaterThan(0);
    });

    it("contains a match", function() {
      const suggestions = response.json.suggestions;
      expect(
        suggestions.some(function(suggestion) {
          return suggestion.name.test(/montreal/i);
        })
      ).toBeTruthy();
    });

    it("contains latitudes and longitudes", function() {
      const suggestions = response.json.suggestions;
      expect(
        suggestions.every(function(suggestion) {
          // warning to type coercions here !
          // if('') console.log('true') => never executed
          return [suggestion.latitude, suggestion.longitude].every(
            field => field !== undefined && field !== ""
          );
        })
      ).toBeTruthy();
    });

    it("contains scores", function() {
      const suggestions = response.json.suggestions;
      expect(
        suggestions.every(function(suggestion) {
          return suggestion.score !== undefined;
        })
      ).toBeTruthy();
    });
  });
});
