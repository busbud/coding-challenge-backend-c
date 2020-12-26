import { ScoreCalculator } from './score.calculator';
import { LocationService } from '../location/location.service';
import { Location } from '../location';

describe('ScoreCalculator', () => {
  const weights = {
    population: 0.3,
    criteria: 0.6,
    nearBy: 0.1,
  };

  const maxPopulation = 1000;
  const maxDistance = 10;

  describe("with caller's location", () => {
    const callerLocation = {
      lng: 1,
      lat: 2,
    };
    const callerGeohash = '9vz';
    const calculator = new ScoreCalculator(
      {
        callerLocation,
        callerGeohash,
        maxPopulation,
        maxDistance,
      },
      { weights },
    );

    beforeEach(() => {
      spyOn(LocationService, 'distanceBetween').and.callFake((l1, l2) =>
        Math.abs(l1.lat + l2.lat - (l1.lng + l2.lng)),
      );
    });

    it("should consider caller's distance when geohash matches", () => {
      expect(
        calculator.getScore({
          geohash: callerGeohash,
          population: maxPopulation,
          searchScore: 1,
          location: {
            ...callerLocation,
            lng: callerLocation.lng + 7,
          },
        }),
      ).toBeCloseTo(0.95);
    });
    it("should not consider caller's distance when geohash doesn't match", () => {
      expect(
        calculator.getScore({
          geohash: 'anotherGeohash',
          population: maxPopulation,
          searchScore: 1,
          location: callerLocation,
        }),
      ).toBeCloseTo(0.9);
    });
  });

  describe("without caller's location", () => {
    const calculator = new ScoreCalculator(
      {
        maxPopulation,
        maxDistance: 0,
      },
      { weights },
    );
    const compute = (score: number, pop: number) =>
      calculator.getScore({
        geohash: (null as unknown) as string,
        population: pop,
        searchScore: score,
        location: (null as unknown) as Location,
      });

    it('should not reach the perfect match', () => {
      expect(compute(1, maxPopulation)).toBeCloseTo(0.9);
    });
    it('should consider population weight', () => {
      expect(compute(0, maxPopulation)).toBeCloseTo(0.3);
    });
    it('should consider criteria weight', () => {
      expect(compute(1, 0)).toBeCloseTo(0.6);
    });

    it('should consider both wieghts', () => {
      expect(compute(0.5, maxPopulation / 2)).toBeCloseTo(0.45);
    });
  });
});
