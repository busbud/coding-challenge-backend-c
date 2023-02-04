import {createServer} from "../src/server/server"
import supertest from 'supertest';
import {IGetCitySuggestion} from "../src/interfaces/interfaces";

describe('GET /suggestions', function() {
  describe('with a non-existent city', function () {
    it('returns a 404 with an empty array of suggestions', async function () {
      const result = await supertest(await createServer())
          .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(404);
      expect(json.suggestions).toBeInstanceOf(Array);
      expect(json.suggestions).toHaveLength(0)
    });
  });

  describe('with a valid city', function () {
    let json: { suggestions: IGetCitySuggestion[] };
    let response: supertest.Response;

    beforeAll(async () =>{
      response = await supertest(await createServer()).get('/suggestions?q=Montreal')
      json = JSON.parse(response.text);
    });

    it('returns a 200', function () {
      expect(response.statusCode).toEqual(200);
    });

    it('returns an array of suggestions', function () {
      expect(json.suggestions).toBeInstanceOf(Array);
      expect(json.suggestions.length).toBeGreaterThanOrEqual(1)
    });
  });

  describe('Validate the shape of the data being returned', function() {
    let json: { suggestions: IGetCitySuggestion[] };
    let response: supertest.Response;

    beforeAll(async () =>{
      response = await supertest(await createServer()).get('/suggestions?q=Montreal')
      json = JSON.parse(response.text);
    });

    it('contains latitudes and longitudes', function () {
      json.suggestions.every(function (suggestion: IGetCitySuggestion) {
          expect(suggestion.latitude).toBeDefined()
          expect(suggestion.longitude).toBeDefined()
      });
    });

    it('contains scores', function () {
      json.suggestions.every(function (suggestion: IGetCitySuggestion) {
        expect(suggestion.score).toBeDefined()
      });
    });

    it('contains a match', function () {
      expect(json.suggestions.map(c => c.name)).toContainEqual(expect.stringContaining("Montreal"))

    });
  });

  describe('returns error when query parameters are invalid', function () {
    it('returns 400 when q is not present', async ()=> {
      const result = await supertest(await createServer())
          .get('/suggestions')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('q must not be empty')
    });

    it('returns 400 when q length is more than 100 characters', async ()=> {
      const s = "s".repeat(100);
      const result = await supertest(await createServer())
          .get(`/suggestions?q=${s}`)
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('q length must not be less than 100')
    });

    it('returns 400 when longitude is not present', async ()=> {
      const result = await supertest(await createServer())
          .get('/suggestions?q=some&latitude=12')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('latitude and longitude must be provided and be valid')
    });

    it('returns 400 when longitude is not valid', async ()=> {
      const result = await supertest(await createServer())
          .get('/suggestions?q=some&longitude=s&latitude=12')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('latitude and longitude must be provided and be valid')
    });

    it('returns 400 when latitude is not present', async ()=> {
      const result = await supertest(await createServer())
          .get('/suggestions?q=some&latitude=12')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('latitude and longitude must be provided and be valid')
    });

    it('returns 400 when latitude is not valid', async ()=> {
      const result = await supertest(await createServer())
          .get('/suggestions?q=some&latitude=s&longitude=12')
      const json = JSON.parse(result.text);
      expect(result.statusCode).toEqual(400);
      expect(json.error).toStrictEqual('latitude and longitude must be provided and be valid')
    });
  });

});
