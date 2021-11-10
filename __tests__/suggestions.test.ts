import { expect } from "chai";
import app from "../app";
import supertest from "supertest";

const request = supertest(app);

describe("GET /suggestions", () => {
  describe("with a non-existent city", () => {
    let response: any;

    before((done) => {
      request
        .get(`/suggestions?q=SomeRandomCityInTheMiddleOfNowhere`)
        .end((err, res) => {
          response = res;
          done(err);
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

  describe("with a valid city", () => {
    let response: any;

    before((done) => {
      request.get("/suggestions?q=Montreal").end((err, res) => {
        response = res;
        done(err);
      });
    });

    it("returns a 200", () => {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length.above(0);
    });

    describe("Validate the shape of the data being returned", () => {
      it("contains latitudes and longitudes", () => {
        expect(response.body.suggestions).to.satisfy((suggestions: any) => {
          return suggestions.every((suggestion: any) => {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      })

      it("contains scores", () => {
        expect(response.body.suggestions).to.satisfy((suggestions: any) => {
          return suggestions.every((suggestion: any) => {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      })
    });

    // ** Got it! :P
    // it("is a gratuitously failing test you should remove to prove you ran the tests", () => {
    //   expect(true).to.equal(false);
    // });

    it("contains a match", () => {
      expect(response.body.suggestions).to.satisfy((suggestions: any) => {
        return suggestions.some((suggestion: any) => {
          console.log(suggestion.name);
          const regex = new RegExp(/montréal.*/i);``
          return regex.test(suggestion.name);
        });
      });
    });
    
  });
});
