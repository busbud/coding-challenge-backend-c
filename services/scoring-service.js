import {
  distanceRangeKM,
  SCORE_WEIGHT_PERCENTAGE,
} from "../utils/constants.js";
import { getLocationBetweenTwoPoints } from "../utils/helpers.js";
import LRUCacheService from "./lru-cache-service.js";

export default class ScoringService {
  constructor(lruCache) {
    this.lruCache = lruCache;
  }

  scoreCategories = {
    ACCURACY: (obj) =>
      (Math.max(0, obj.result.accuracy / 100) *
        SCORE_WEIGHT_PERCENTAGE.ACCURACY) /
      100,
    LOCATION: (obj) =>
      (this.locationProximity(obj.result, obj.latitude, obj.longitude) *
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

      scoredResults.push({
        name: `${result.name}, ${result.state}, ${result.country}`,
        latitude: result.latitude,
        longitude: result.longitude,
        score: Math.min(maxScore, score).toFixed(1),
      });
    }

    return (scoredResults || []).sort((a, b) => b.score - a.score);
  }

  locationProximity = (result, incomingLatitude, incomingLongitude) => {
    if (incomingLatitude && incomingLongitude) {
      const distance = getLocationBetweenTwoPoints(
        result,
        Number(incomingLatitude),
        Number(incomingLongitude)
      );

      const distanceKM = distance / 1000;

      return distanceKM > distanceRangeKM ? 0 : 1;
    }

    return 1;
  };
}
