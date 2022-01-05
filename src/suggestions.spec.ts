import { app } from './app';
import * as supertest from 'supertest';


describe('GET /suggestions', () => {
  let request: supertest.SuperTest<any>;

  beforeAll(async () => {
    await app.ready();
    request = supertest(app.server);
  });

  afterAll(() => app.close());

  describe('with a non-existent city', () => {
    let response: any;

    beforeAll(async () => {
      response = await request.get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere');
    });

    it('returns a 404', () => {
      expect(response.statusCode).toEqual(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions).toHaveLength(0);
    });
  });

  describe('with a valid city',  () => {
    let response;

    beforeAll(async () => {
      response = await request.get('/suggestions?q=Montreal');
    });

    it('returns a 200', () => {
      expect(response.statusCode).toEqual(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).toBeInstanceOf(Array);
      expect(response.json.suggestions.length).toBeGreaterThan(0);
    });

    describe.skip('Validate the shape of the data being returned', () => {
      it('contains latitudes and longitudes', () => {
        const hasLatLng = response.json.suggestions.every(s => s.latitude && s.longitude);
        expect(hasLatLng).toBeTruthy();
      });

      it('contains scores', () => {
        const hasScores = response.json.suggestions.every(s => !!s.score);
        expect(hasScores).toBeTruthy();
      });
    });

    it('contains a match', () => {
      expect(response.json.suggestions.some(s => s.name.test(/montreal/i))).toBeTruthy();
    });
  });
});
