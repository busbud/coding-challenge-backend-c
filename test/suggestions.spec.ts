/* eslint-disable @typescript-eslint/no-explicit-any */
import { CitiesDTO } from "../src/services/cities.definition";
import request from "supertest";
import unidecode from "unidecode";

import pkg from "chai";
const { expect } = pkg;

import app from "../src/app";

describe("GET /suggestions", function () {
  describe("with a non-existent city", function () {
    let response: any;

    before(function (done) {
      request(app)
        .get("/suggestions?q=SomeRandomCityInTheMiddleOfNowhere")
        .end(function (err, res) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 404", function () {
      expect(response.statusCode).to.equal(404);
    });

    it("returns an empty array of suggestions", function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe("with a valid city", function () {
    let response: any;

    before(function (done) {
      request(app)
        .get("/suggestions?q=Montreal")
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it("returns a 200", function () {
      expect(response.statusCode).to.equal(200);
    });

    it("returns an array of suggestions", function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    describe("Validate the shape of the data being returned", function () {
      it("contains latitudes and longitudes", function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions: CitiesDTO[]) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it("contains scores", function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions: CitiesDTO[]) {
          return suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude);
        });
      });
    });

    it("contains a match", function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions: CitiesDTO[]) {
        return suggestions.some((suggestion) => /montreal/i.test(unidecode(suggestion.name)));
      });
    });

    describe("Validate the shape of the query param being passed in the request", function () {
      it("throws error if q for city name is not provided", (done) => {
        request(app)
          .get("/suggestions?lat=-73&lon=43")
          .end((err, res) => {
            expect(res?.status).to.equal(400);
            expect(res?.body?.message).to.contain("Invalid Query Parameters");
            done(err);
          });

        expect(response.json.suggestions).to.satisfy(function (suggestions: CitiesDTO[]) {
          return suggestions.every((suggestion) => suggestion.latitude && suggestion.longitude);
        });
      });

      it("throws error if only longitude is provided without a latitude", (done) => {
        request(app)
          .get("/suggestions?q=Montreal&lon=43")
          .end((err, res) => {
            expect(res?.status).to.equal(400);
            expect(res?.body?.message).to.contain("Invalid Query Parameters");
            done(err);
          });
      });

      it("throws error if only latitude is provided without a longitude", (done) => {
        request(app)
          .get("/suggestions?q=Montreal&lat=-75")
          .end((err, res) => {
            expect(res?.status).to.equal(400);
            expect(res?.body?.message).to.contain("Invalid Query Parameters");
            done(err);
          });
      });
    });
  });
});
