var chai = require("chai");
var chaiIncreasing = require("chai-increasing");
var expect = chai.expect;
var app = require("../../app");
var request = require("supertest")(app);
var knex = require("../../utils/knexclient");
var model = require("../resources/suggestion_it").model;

chai.use(chaiIncreasing);

describe("GET /suggestions", function() {
  this.beforeEach(async function() {
    await knex(model.table).truncate();
  });

  /**
   * TEST - with a non-existent city
   */
  describe("with a non-existent city", function() {
    var response;

    before(function(done) {
      knex(model.table)
        .insert(model.montreal)
        .then(() => {
          request
            .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
            .end(function(err, res) {
              response = res;
              response.json = JSON.parse(res.text);
              done(err);
            });
        })
        .catch(err => done(err));
    });

    it("returns a 404", function() {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  /**
   * TEST - with a valid city
   */
  describe("with a valid city", function() {
    var response;

    before(function(done) {
      knex(model.table)
        .insert(model.montreal)
        .then(() => {
          request.get("/suggestions?q=Montreal").end(function(err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
        });
    });

    it("returns a 200", function() {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    describe.skip("Validate the shape of the data being returned", function() {
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
          return suggestion.name.match(/montreal/i);
        });
      });
    });
  });

  describe("with vancouver and vancleave suggestions from montreal", function() {
    before(function(done) {
      knex(model.table)
        .insert([model.montreal, model.vancouver, model.vancleave])
        .then(() => {
          request.get("/suggestions?q=vanc").end(function(err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
        });
    });

    it("returns a 200", function() {
      expect(response.statusCode).to.equal(200);
    });

    it("has multiple values", function() {
      expect(response.json.suggestions)
        .to.be.an("array")
        .that.has.lengthOf(2);
    });

    it("should have vancleave with higher score than vancouver", function() {
      expect(response.json.suggestions)
      .to.be.strictly.decreasing;
    });
  });

  describe("with city outside US and CA", function() {
    before(function(done) {
      knex(model.table)
        .insert(model.campinas)
        .then(() => {
          request.get("/suggestions?q=campinas").end(function(err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
        });
    });

    it("returns a 404", function() {
      expect(response.statusCode).to.equal(404);
    });
  });

  describe("with city small city population under 5000", function() {
    before(function(done) {
      knex(model.table)
        .insert(model.cobalto)
        .then(() => {
          request.get("/suggestions?q=cobalto").end(function(err, res) {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
        });
    });

    it("returns a 404", function() {
      expect(response.statusCode).to.equal(404);
    });
  });
});
