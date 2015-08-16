'use strict';

/* eslint-env mocha */

import {expect} from 'chai';
import app from '../app';
import request from 'supertest-as-promised';

let response;

const get = url => () =>
  request(app)
    .get(url)
    .then(res => Object.assign(res, {json: JSON.parse(res.text)}))
    .then(res => response = res);

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    before(get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere'));

    it('returns a 404', () => {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with a valid city', () => {
    before(get('/suggestions?q=Montreal'));

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length.above(0);
    });

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy(suggestions =>
        suggestions.some(suggestion =>
          suggestion.name.test(/montreal/i)
        )
      );
    });

    it('contains latitudes and longitudes', () => {
      expect(response.json.suggestions).to.satisfy(suggestions =>
        suggestions.every(suggestion =>
          suggestion.latitude && suggestion.longitude
        )
      );
    });

    it('contains scores', () => {
      expect(response.json.suggestions).to.satisfy(suggestions =>
        suggestions.every(suggestion =>
          suggestion.latitude && suggestion.longitude
        )
      );
    });
  });
});
