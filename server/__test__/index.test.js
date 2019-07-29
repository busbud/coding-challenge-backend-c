const db = require('../../db');
const config = require('../../config').get();
const logger = require('../../logger')(config);
const server = require('../server');
const es = require('@elastic/elasticsearch');
const request = require('supertest');


describe('server', () => {
  let serverInstance;
  let client;

  beforeEach(async () => {
    try {
      client = await db.getClient(config.db, es);
      serverInstance = await server.start(config, logger, client, db);
    }
    catch (err) {
      console.error(err);
    }

  });

  afterEach(() => {
    serverInstance.close();
  });

  test('no root endpoint', async () => {
    const res = await request(serverInstance).get('/');
    expect(res.statusCode).toEqual(404);
  });

  describe('malformed queries [400]', () => {
    test('malformed queries 1', async () => {
      const res = await request(serverInstance).get('/suggestions');
      expect(res.statusCode).toEqual(400);
    });

    test('malformed queries 2', async () => {
      const res = await request(serverInstance).get('/suggestions?q=');
      expect(res.statusCode).toEqual(400);
    });
  });

  describe('non existent city name', () => {
    test('should return 404', async () => {
      const res = await request(serverInstance).get('/suggestions?q=thisisnotacityname');
      expect(res.statusCode).toEqual(404);
      expect(res.body).toEqual([]);
    });
  });

  describe('should return a match', () => {
    test('response must be valid', async () => {
      const res = await request(serverInstance).get('/suggestions?q=montr');

      const match = res.body.some(function (suggestion) {
        return suggestion.name.match(/montréal/i);
      });

      const hasCoords = res.body.every((suggestion) => {
        return suggestion.latitude && suggestion.longitude;
      });

      const hasScore = res.body.every((suggestion) => {
        return suggestion.score;
      });

      expect(hasScore).toBeTruthy();
      expect(hasCoords).toBeTruthy();
      expect(match).toBeTruthy();
      expect(res.statusCode).toEqual(200);
      expect(res.body.length > 0).toBeTruthy();
    });
  });
});

