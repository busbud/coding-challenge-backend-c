import { CityQueryResult } from '../cities/interfaces/city-query-result';

export class ScoreCalculator {
  private readonly maxPopulation: number;

  constructor({ maxPopulation }) {
    this.maxPopulation = maxPopulation;
  }

  /**
   * Population should skew the score as the searchScore approaches to 0
   */
  getScore({ searchScore, population }: CityQueryResult) {
    const POPULATION_WEIGHT = 1 - searchScore;
    return searchScore + POPULATION_WEIGHT * (population / this.maxPopulation);
  }
}
