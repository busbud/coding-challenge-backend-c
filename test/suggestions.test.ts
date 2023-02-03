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
      json.suggestions.some((c: IGetCitySuggestion) => {
        expect(c.name).toMatch(/montreal/i)
      })

    });
  });

});
