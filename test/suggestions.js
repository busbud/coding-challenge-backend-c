var expect = require("chai").expect;
var app = require("../app");
var request = require("supertest")(app);
const waitForApp = require("../src/dbUtils").waitForApp;
const redisClient = require("../src/cache").redisClient;
const getRedisKey = require("../src/suggestions/dataUtils").getRedisKey;

const callApi = (q, long, lat) => {
  url = "/suggestions?q=" + q;
  url += (long) ? "&long=" + long : "";
  url += (lat) ? "&lat=" + lat : "";
  return new Promise((resolve, reject) => {
    request.get("/suggestions?q=" + q).end(function(err, res) {
      if (err) {
        reject(err);
      } else {
        response = res;
        response.json = JSON.parse(res.text);
        resolve(response);
      }
    });
  });
};

describe("GET /suggestions", function() {
  this.timeout(7000);

  before(done => {
    waitForApp(1000, 5)
      .then(() => {
        redisClient().flushall();
        done();
      })
      .catch(err => {
        done(err);
      });
  });

  describe("with a non-existent city", () => {
    let response;

    before(async () => {
      return (response = await callApi("SomeRandomCityInTheMiddleOfNowhere"));
    });

    it("returns a 404", () => {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe("with a valid city", () => {
    let response;

    before(async () => {
      return (response = await callApi("Montreal"));
    });

    it("returns a 200", () => {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it("contains a match", () => {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.some(function(suggestion) {
          return new RegExp(/montreal/, "i").test(suggestion.name);
        });
      });
    });

    it("contains latitudes and longitudes", () => {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("contains scores", () => {
      expect(response.json.suggestions).to.satisfy(function(suggestions) {
        return suggestions.every(function(suggestion) {
          return suggestion.latitude && suggestion.longitude;
        });
      });
    });

    it("has been store in redis", () => {
      redisClient().get(getRedisKey({q: "Montreal"}), (err, reply) => {
        expect(reply).to.not.equal(null);
      });
    });
  });

  describe("match city name with space against non spaced input", () => {
    let response;
    before(async () => {
      return (response = await callApi("newY"));
    });

    it("returns an array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });
  });

  describe("works with geo input", () => {
    let response;
    before(async () => {
      return (response = await callApi("newY", 1, 2));
    });

    it("returns an array of suggestions", function() {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });
  });

});
