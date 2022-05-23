import { Server } from 'http';
import supertest, { SuperTest, Test } from 'supertest';
import app from 'app';
import db from 'service/database';
import { migration as migrationConfig } from 'config/database';
import { City } from 'model/city';
import { Country } from 'model/country';
import { State } from 'model/state';

let server: Server;
let request: SuperTest<Test>;
let response: any;

beforeAll(async () => {
  await db.migrate.latest(migrationConfig);

  await db<City>('city').insert([{
    id: 5128581,
    ascii: 'New York City',
    name: 'New York City',
    country: 'US',
    admin1: 'NY',
    lat: 40.714270,
    long: -74.005970,
    population: 8175133,
  }]);

  await db<City>('city').insert([{
    id: 6077243,
    ascii: 'Montreal',
    name: 'Montréal',
    country: 'CA',
    admin1: '10',
    lat: 45.508840,
    long: -73.587810,
    population: 3268513,
  }]);

  await db<City>('city').insert([{
    id: 6077128,
    ascii: 'Mont-Laurier',
    name: 'Mont-Laurier',
    country: 'CA',
    admin1: '10',
    lat: 46.550110,
    long: -75.499300,
    population: 13405,
  }]);

  await db<Country>('country').insert([{
    id: 6252001,
    iso: 'US',
    country: 'United States',
  }]);

  await db<Country>('country').insert([{
    id: 6251999,
    iso: 'CA',
    country: 'Canada',
  }]);

  await db<State>('state').insert([{
    id: 5128638,
    country: 'US',
    admin1: 'NY',
    name: 'New York',
  }]);

  await db<State>('state').insert([{
    id: 6115047,
    country: 'CA',
    admin1: '10',
    name: 'Quebec',
  }]);

  server = app.listen();
  request = supertest(server);
});

afterAll(async () => {
  server.close();

  await db.migrate.rollback(migrationConfig);
  await db.destroy();
});

describe('GET /suggestions', () => {
  describe('With a missing search term', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With an empty search term', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions?q=');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With an incorrect latitude', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions?q=SomeRandomCity&latitude=360&longitude=45');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With an incorrect longitude', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions?q=SomeRandomCity&latitude=45&longitude=360');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With a missing latitude', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions?q=SomeRandomCity&longitude=45');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With a missing longitude', () => {
    test('Returns a 400', async () => {
      response = await request.get('/suggestions?q=SomeRandomCity&latitude=45');
      expect(response.status).toBe(400);
    });
    test('Returns an error message', async () => {
      expect(response.body.error).toMatch(/.+/);
    });
  });

  describe('With a non-existent city', () => {
    test('Returns a 404', async () => {
      response = await request.get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere');
      expect(response.status).toBe(404);
    });
    test('Returns an empty array of suggestions', async () => {
      expect(response.body.suggestions).toBeInstanceOf(Array);
      expect(response.body.suggestions.length).toBe(0);
    });
  });

  describe('With a valid city', () => {
    test('Returns a 200', async () => {
      response = await request.get('/suggestions?q=Montreal');
      expect(response.status).toBe(200);
    });
    test('Returns an array of suggestions', async () => {
      expect(response.body.suggestions).toBeInstanceOf(Array);
      expect(response.body.suggestions.length).toBeGreaterThan(0);
    });
    test('Suggestions have a score', async () => {
      expect(response.body.suggestions.every((suggestion: any) => suggestion.score)).toBe(true);
    });
    test('Suggestions have a name', async () => {
      expect(response.body.suggestions.every((suggestion: any) => suggestion.name)).toBe(true);
    });
    test('Suggestions have a latitude and longitude', async () => {
      expect(response.body.suggestions.every(
        (suggestion: any) => suggestion.latitude && suggestion.longitude,
      )).toBe(true);
    });
    test('Suggestions contain a match', async () => {
      expect(response.body.suggestions.some((suggestion: any) => /montréal/i.test(suggestion.name))).toBe(true);
    });
  });

  describe('With a valid city and location', () => {
    test('Returns a 200', async () => {
      response = await request.get('/suggestions?q=Montreal&latitude=45&longitude=45');
      expect(response.status).toBe(200);
    });
    test('Returns an array of suggestions', async () => {
      expect(response.body.suggestions).toBeInstanceOf(Array);
      expect(response.body.suggestions.length).toBeGreaterThan(0);
    });
  });

  describe('With a multi-match query string', () => {
    test('Returns a 200', async () => {
      response = await request.get('/suggestions?q=Mont');
      expect(response.status).toBe(200);
    });
    test('Returns an array of at least 2 matches', async () => {
      expect(response.body.suggestions).toBeInstanceOf(Array);
      expect(response.body.suggestions.length).toBeGreaterThan(1);
    });
  });
});
