import chai, { expect } from "chai";
import chaiHttp from "chai-http";
import app from "../app.js";

chai.use(chaiHttp);
chai.should();

describe("GET /suggestions", () => {
  describe("with a non-existent city", () => {
    var response;

    before((done) => {
      chai
        .request(app)
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 404", () => {
      console.log(response.json);
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe("with a valid city", () => {
    var response;

    before((done) => {
      chai
        .request(app)
        .get("/suggestions?q=Montreal")
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 200", () => {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it("contains latitudes and longitudes", () => {
      response.json.suggestions.forEach((suggestion) => {
        expect(parseFloat(suggestion.latitude)).to.be.within(-180, 180);
        expect(parseFloat(suggestion.longitude)).to.be.within(-180, 180);
      });
    });

    it("contains scores", () => {
      response.json.suggestions.forEach((suggestion) => {
        expect(suggestion.score).to.be.above(0);
      });
    });

    it("contains a match", () => {
      response.json.suggestions.forEach((suggestion) => {
        expect(suggestion.name).to.include("ontreal");
      });
    });
  });
});
