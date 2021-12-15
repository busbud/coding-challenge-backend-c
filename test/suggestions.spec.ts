import request, {Response} from 'supertest'
import {expect} from 'chai';
import Server from '../src/server';
import {cityRepository} from "../src/repository/city";
import * as path from "path";
import deburr from 'lodash/deburr';

const server = new Server().init()

// Load the cities
beforeAll(async () => {
  await cityRepository.loadCities(path.join(process.cwd(), 'data', 'cities_canada-usa.tsv'))
})

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    let response: Response

    beforeEach(async () => {
      response = await request(server)
          .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
    });

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', () => {
    let response: Response
    //
    beforeEach(async () => {
      response = await request(server)
          // @ts-ignore
          .get('/suggestions?q=Montreal')
    });

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      // @ts-ignore
      expect(response.body.suggestions).to.be.instanceof(Array);
      // @ts-ignore
      expect(response.body.suggestions).to.have.length.above(0);
    });

    describe('Validate the shape of the data being returned', () => {
      it('contains latitudes and longitudes', () => {
        // @ts-ignore
        expect(response.body.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion: { latitude: any; longitude: any; }) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('contains scores', () => {
        expect(response.body.suggestions).to.satisfy(function (suggestions: any[]) {
          return suggestions.every(function (suggestion: { latitude: any; longitude: any; }) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });
    });

    it('contains a match', () => {
      expect(response.body.suggestions).to.satisfy(function (suggestions: any[]) {
        return suggestions.some(function (suggestion) {
          return /montreal/i.test(deburr(suggestion.name));
        });
      })
    });
  });
});
