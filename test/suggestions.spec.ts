import { setupApp } from '../src/app';

import { Express } from 'express';
import request from 'supertest';

let app: Express;

describe('GET /suggestions', () => {
  beforeAll(async () => {
    app = setupApp();
  });

  describe('with a non-existent city', function () {
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
});
