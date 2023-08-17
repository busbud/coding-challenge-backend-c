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
  describe('with no parameters', () => {
    const query = '/suggestions';

    it('returns a 400', async () => {
      const res = await supertest(app).get(query);
      expect(res.statusCode).toBe(400);
    });
  });

  describe('with an invalid search term', () => {
    const query = '/suggestions?q=789';

    it('returns a 404', async () => {
      const res = await supertest(app).get(query);
      expect(res.statusCode).toBe(404);
    });
  });

  describe('with a valid search term, but invalid latitude and longitude', () => {
    const query = '/suggestions?q=Londo&latitude=900&longitude=jaskarn';

    it('returns a 400', async () => {
      const res = await supertest(app).get(query);
      expect(res.statusCode).toBe(400);
    });
  });

  describe('with a valid search term, and latitude but no longitude', () => {
    const query = '/suggestions?q=Londo&latitude=89';

    it('returns a 400', async () => {
      const res = await supertest(app).get(query);
      expect(res.statusCode).toBe(400);
    });
  });

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
          const { latitude, longitude } = suggestion;
          expect(latitude).toBeTruthy();
          expect(longitude).toBeTruthy();
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

  describe('with a valid city and coordinate', () => {
    const query = '/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163';

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
          const { latitude, longitude } = suggestion;
          expect(latitude).toBeTruthy();
          expect(longitude).toBeTruthy();
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
          /london/i.test(suggestion.name),
        );

        expect(hasMatchingSuggestion).toBe(true);
      });
    });
  });
});

afterAll(async () => {
  await redisClient.quit();
});
