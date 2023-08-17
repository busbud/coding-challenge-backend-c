import app from '../src/server';
import supertest from 'supertest';

import redisClient from '../src/modules/redis';

import type { ScoredSuggestion } from '../src/handlers/lib';

describe('GET /', () => {
  it('should send back a hello message', async () => {
    const res = await supertest(app).get('/');
    const message = res.body.message as string;

    expect(message).toBe('Hey there welcome to Busbud! :)');
  });
});

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    const query = '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere';

    it('returns a 404', async () => {
      const res = await supertest(app).get(query);

      expect(res.statusCode).toBe(404);
    });

    it('returns an empty array of suggestions', async () => {
      const res = await supertest(app).get(query);
      const suggestions = res.body.suggestions as ScoredSuggestion[];

      expect(suggestions).toBeInstanceOf(Array);
      expect(suggestions).toHaveLength(0);
    });
  });

  describe('with a valid city', () => {
    const query = '/suggestions?q=Montreal';

    it('returns a 200', async () => {
      const res = await supertest(app).get(query);

      expect(res.statusCode).toBe(200);
    });

    it('returns an array of suggestions', async () => {
      const res = await supertest(app).get(query);
      const suggestions = res.body.suggestions as ScoredSuggestion[];

      expect(suggestions).toBeInstanceOf(Array);
      expect(suggestions.length).toBeGreaterThan(0);
    });

    describe('validate the shape of the data being returned', () => {
      it('contains latitudes and longitudes', async () => {
        const res = await supertest(app).get(query);
        const suggestions = res.body.suggestions as ScoredSuggestion[];

        for (const suggestion of suggestions) {
          const { lat, long } = suggestion;
          expect(lat).toBeTruthy();
          expect(long).toBeTruthy();
        }
      });

      it('contains scores', async () => {
        const res = await supertest(app).get(query);
        const suggestions = res.body.suggestions as ScoredSuggestion[];

        for (const suggestion of suggestions) {
          const { score } = suggestion;
          expect(score).toBeTruthy();
        }
      });

      it('contains a match', async () => {
        const res = await supertest(app).get(query);
        const suggestions = res.body.suggestions as ScoredSuggestion[];

        const hasMatchingSuggestion = suggestions.some((suggestion) =>
          /montréal/i.test(suggestion.name),
        );

        expect(hasMatchingSuggestion).toBe(true);
      });
    });
  });
});

afterAll(async () => {
  await redisClient.quit();
});
