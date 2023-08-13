import { Express } from 'express';
import request from 'supertest';
import { toSatisfyAll, toSatisfyAny } from 'jest-extended';

import { setupApp } from '../src/app';

expect.extend({ toSatisfyAll, toSatisfyAny });

let app: Express;

describe('GET /suggestions', () => {
  beforeAll(async () => {
    app = setupApp();
  });

  describe('with a non-existent city', () => {
    let response: request.Response;

    beforeAll((done) => {
      request(app)
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end((err, res) => {
          response = res;
          done(err);
        });
    });

    test('returns a 404', () => {
      expect(response.statusCode).toEqual(404);
    });

    test('returns an empty array of suggestions', () => {
      const json = JSON.parse(response.text);

      expect(json.suggestions).toBeInstanceOf(Array);
      expect(json.suggestions).toHaveLength(0);
    });
  });

  describe('with a valid city', () => {
    let response: request.Response;

    beforeAll((done) => {
      request(app)
        .get('/suggestions?q=Montreal')
        .end((err, res) => {
          response = res;
          done(err);
        });
    });

    test('returns a 200', () => {
      expect(response.statusCode).toEqual(200);
    });

    test('returns an array of suggestions', () => {
      const json = JSON.parse(response.text);

      expect(json.suggestions).toBeInstanceOf(Array);
      expect(json.suggestions.length).toBeGreaterThan(0);
    });

    describe('Validate the shape of the data being returned', () => {
      test('contains latitudes and longitudes', () => {
        const json = JSON.parse(response.text);

        expect(json.suggestions).toSatisfyAll(
          (suggestion) => suggestion.latitude && suggestion.longitude
        );
      });

      test('contains scores', function () {
        const json = JSON.parse(response.text);

        expect(json.suggestions).toSatisfyAll((suggestion) => suggestion.score);
      });
    });

    test('contains a match', function () {
      const json = JSON.parse(response.text);

      expect(json.suggestions).toSatisfyAny((suggestion) =>
        /montréal/i.test(suggestion.name)
      );
    });
  });

  describe('with a valid city and location', () => {
    let response: request.Response;

    beforeAll((done) => {
      request(app)
        .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
        .end((err, res) => {
          response = res;
          done(err);
        });
    });

    test('returns a 200', () => {
      expect(response.statusCode).toEqual(200);
    });

    test('returns an array of suggestions', () => {
      const json = JSON.parse(response.text);

      expect(json.suggestions).toBeInstanceOf(Array);
      expect(json.suggestions.length).toBeGreaterThan(0);
    });

    describe('Validate the shape of the data being returned', () => {
      test('contains latitudes and longitudes', () => {
        const json = JSON.parse(response.text);

        expect(json.suggestions).toSatisfyAll(
          (suggestion) => suggestion.latitude && suggestion.longitude
        );
      });

      test('contains scores', function () {
        const json = JSON.parse(response.text);

        expect(json.suggestions).toSatisfyAll((suggestion) => suggestion.score);
      });
    });

    test('contains a match', function () {
      const json = JSON.parse(response.text);

      expect(json.suggestions).toSatisfyAny((suggestion) =>
        /london/i.test(suggestion.name)
      );
    });
  });
});
