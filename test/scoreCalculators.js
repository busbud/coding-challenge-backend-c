const expect = require('chai').expect;
const {calculateSimpleScore, getDistanceBasedScoreCalculator} = require('../scoreCalculators');

describe('scoreCalculators', () => {
  describe('calculateSimpleScore', () => {
    it('should invert the provided score value', () => {
      expect(calculateSimpleScore({score: 0.05})).equals(0.95);
    });
  });

  describe('getDistanceBasedScoreCalculator', () => {
    it('should return a function', () => {
      expect(getDistanceBasedScoreCalculator(142, 24)).is.instanceOf(Function);
    });

    describe('function created by getDistanceBasedScoreCalculator', () => {
      let calculator;
      beforeEach(() => {
        calculator = getDistanceBasedScoreCalculator(37.12898, -84.08326);
      });

      it('should calculate the score based on the distance weighted against the name', () => {
        expect(calculator({
          item: {
            lat: 37.12898,
            long: -84.08326
          },
          score: 1
        })).equal(0.6);
      });
    });
  });
});