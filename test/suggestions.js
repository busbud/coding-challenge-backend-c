'use strict';

/* eslint-env mocha */

import {expect} from 'chai';
import suggestions from '../src/suggestions';
import SuggestionsError from '../src/suggestions/error';

describe('suggestions()', () => {
  describe('without required parameter', () => {
    it('should throw an error', () => {
      expect(suggestions).to.throw(SuggestionsError);
    });
  });

  describe('without query', () => {
    it('should throw an error', () => {
      expect(() => suggestions({})).to.throw(SuggestionsError);
    });
  });

  describe('with a non-existent city', () => {
    const result = suggestions({q: 'SomeRandomCityInTheMiddleOfNowhere'});

    it('returns an empty array of suggestions', () => {
      expect(result).to.be.instanceof(Array);
      expect(result).to.have.length(0);
    });
  });

  describe('with a valid city', () => {
    const result = suggestions({q: 'Montreal'});

    it('returns an array of suggestions', () => {
      expect(result).to.be.instanceof(Array);
      expect(result).to.have.length.above(0);
    });

    it('contains a match', () => {
      expect(result).to.satisfy(suggestions =>
        suggestions.some(suggestion =>
          suggestion.name.match(/montr[eé]al/i)
        )
      );
    });

    it('contains latitudes and longitudes', () => {
      expect(result).to.satisfy(suggestions =>
        suggestions.every(suggestion =>
          suggestion.latitude && suggestion.longitude
        )
      );
    });

    it('contains scores', () => {
      expect(result).to.satisfy(suggestions =>
        suggestions.every(suggestion =>
          'score' in suggestion
        )
      );
    });
  });

  describe('with coordinates hint', () => {
    const result = suggestions({
      q:         'London',
      latitude:  42,
      longitude: -70
    });

    it('improves the score', () => {
      expect(result.slice(0, 3)).to.satisfy(suggestions =>
        suggestions.some(suggestion =>
          suggestion.name.match(/londonderry/i)
        )
      );
    });
  });
});
