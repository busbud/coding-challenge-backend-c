import { expect } from 'chai';
import supertest, { Response } from 'supertest';
import Suggestion from '../../src/types/Suggestion';
import app from '../../src/app';
import { response } from 'express';

const request = supertest(app);

describe('GET /suggestions', function () {
  describe('with a non-existent city', function () {
    let response;

    before(function (done) {
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err: any, res: Response) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('With a valid city', function () {
    let response;

    before(function (done) {
      request
        .get('/suggestions?q=Montreal')
        .end(function (err: any, res: Response) {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    describe('Validate the shape of the data being returned', function () {

      it('contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions: Suggestion[]) {
          return suggestions.every(function (suggestion: Suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions: Suggestion[]) {
          return suggestions.every(function (suggestion: Suggestion) {
            return suggestion.score !== undefined;
          });
        })
      });
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions: Suggestion[]) {
        return suggestions.some(function (suggestion: Suggestion) {
          return suggestion.name.match(/montréal/i);
        });
      })
    });
  });

  describe('Validate params', () => {

    it('returns invalid latitude', (done) => {
      request.get('/suggestions?q=Montreal&latitude=hey&longitude=-79.4163')
        .expect(400)
        .end((err: any, res: Response) => {
          expect(res.text).to.be.equals('Latitude must be a number');
          done();
        });
    });

    it('returns invalid longitude', (done) => {
      request.get('/suggestions?q=Montreal&latitude=43.70011&longitude=hey')
        .expect(400)
        .end((err: any, res: Response) => {
          expect(res.text).to.be.equals('Longitude must be a number');
          done();
        });
    });

    it('returns empty when q is missing', (done) => {
      request.get('/suggestions?q=')
        .expect(404)
        .end((err: any, res: Response) => {
          const json = JSON.parse(res.text);
          expect(json.suggestions).to.be.instanceof(Array);
          expect(json.suggestions).to.have.length(0);
          done();
        });
    });

  });

});
