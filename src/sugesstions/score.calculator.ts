import { CityQueryResult } from '../cities';
import { LocationService } from '../location/location.service';
import { Location } from '../location';

export type ScoreCalculatorParams = {
  maxPopulation: number;
  maxDistance: number;
  callerGeohash?: string;
  callerLocation?: Location;
};

export type ScoreCalculatorConfig = {
  weights: {
    nearBy: number;
    criteria: number;
    population: number;
  };
};

export interface IScoreCalculator {
  getScore(city: CityQueryResult);
}

type RelevantCityParams = Pick<
  CityQueryResult,
  'searchScore' | 'population' | 'geohash' | 'location'
>;

export class ScoreCalculator implements IScoreCalculator {
  constructor(
    private readonly params: ScoreCalculatorParams,
    private readonly config: ScoreCalculatorConfig,
  ) {}

  /**
   * Population should skew the score as the searchScore approaches to 0
   */
  getScore({ searchScore, population, geohash, location }: RelevantCityParams) {
    const PW = this.config.weights.population;
    const CW = this.config.weights.criteria;
    const DW = this.config.weights.nearBy;

    const partial =
      (PW * population) / this.params.maxPopulation + CW * searchScore;

    if (this.params.callerLocation && this.isNearCallerLocation(geohash)) {
      const distance =
        LocationService.distanceBetween(location, this.params.callerLocation) /
        this.params.maxDistance;
      return partial + DW * (1 - distance);
    }
    return partial;
  }

  private isNearCallerLocation(geohash: string): boolean {
    return this.params.callerGeohash === geohash;
  }
}
