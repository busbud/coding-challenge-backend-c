import {
  CANADA_PROVINCES,
  COUNTRIES,
  SCORE_WEIGHT_PERCENTAGE,
} from "../utils/constants.js";
import { locationProximity } from "../utils/helpers.js";

/**
 * ScoringService: service to manage the scoring algorithm
 */
export default class ScoringService {
  constructor(lruCache) {
    this.lruCache = lruCache;
  }

  scoreCategories = {
    ACCURACY: ({ result: { accuracy } }) =>
      (Math.max(0, accuracy / 100) * SCORE_WEIGHT_PERCENTAGE.ACCURACY) / 100,
    LOCATION: ({ result, latitude, longitude }) =>
      (locationProximity(result, latitude, longitude) *
        SCORE_WEIGHT_PERCENTAGE.LOCATION_PROXIMITY) /
      100,
    FREQUENTLY_USED: ({ result }) => {
      const key = `${result.name}, ${result.state}, ${result.country}`;
      const isRecentlyUsed = this.lruCache.isRecentlyUsed(key);
      const frequencyScore = isRecentlyUsed ? 1 : 0;
      return (
        frequencyScore * (SCORE_WEIGHT_PERCENTAGE.FREQUENTLY_SEARCHED / 100)
      );
    },
  };
  /**
   *
   * @param {Map} results
   * @param {number} latitude
   * @param {number} longitude
   * @returns
   */
  score(results, latitude, longitude) {
    if (!results) return [];
    if (!results.size) return [];

    const scoredResults = [];

    for (const result of results.values()) {
      const initialScore = 0;
      const maxScore = 1;

      const score = Object.keys(this.scoreCategories).reduce(
        (prev, current) => {
          return (
            prev +
            this.scoreCategories[current]({ result, latitude, longitude })
          );
        },
        initialScore
      );

      const provinceOrStateName =
        result.country === COUNTRIES.Canada
          ? CANADA_PROVINCES[result.state].init
          : result.state;

      scoredResults.push({
        name: `${result.name}, ${provinceOrStateName}, ${result.country}`,
        latitude: result.latitude,
        longitude: result.longitude,
        score: Math.min(maxScore, score).toFixed(1),
      });
    }

    return (scoredResults || []).sort((a, b) => b.score - a.score);
  }
}
