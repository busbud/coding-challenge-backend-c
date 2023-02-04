import {GeoPosition} from 'geo-position.ts';
import {ICityRawData, IGetCitySuggestion} from '../interfaces/interfaces';
import {getCityDetailString} from './geography.util';
/* eslint @typescript-eslint/no-var-requires: "off" */
const distance = require('jaro-winkler');

const SCORE_THRESHOLD = 0.7;

/** Calculates score for distance range on the scale of 0 to 1 */
function calculateScoreForDistance(distances: number[]) {
  const min = Math.min(...distances);
  const max = Math.max(...distances);
  const range = max - min;
  return distances.map((dis) => {
    return 1 - (dis - min) / range;
  });
}

/** Calculates score by factoring in string similarity and distance score on the scale of 0 to 1 */
function updateScoreBasedOnDistance(
  citySuggestions: IGetCitySuggestion[],
  distanceScores: number[],
) {
  const distanceWeight = 0.5;
  const spellingWeight = 0.5;
  return citySuggestions
    .map((suggestion, idx) => {
      const score = suggestion.score * spellingWeight + distanceScores[idx] * distanceWeight;
      return {
        ...suggestion,
        score: Number(score.toFixed(1)),
      };
    })
    .filter((x) => x.score > SCORE_THRESHOLD);
}

/** Calculates score for distance on the scale of 0 to 1 and updates the existing similarity score */
export function scoreByDistance(
  citySuggestions: IGetCitySuggestion[],
  latitude: string,
  longitude: string,
) {
  const locationA = new GeoPosition(Number(latitude), Number(longitude));
  const distances = citySuggestions.map((suggestion) => {
    const location = new GeoPosition(Number(suggestion.latitude), Number(longitude));
    return locationA.Distance(location);
  });
  const scores = calculateScoreForDistance(distances);
  return updateScoreBasedOnDistance(citySuggestions, scores);
}

/** Sorts object on descending order based on score */
export function sortByScore(cities: IGetCitySuggestion[]): IGetCitySuggestion[] {
  return cities.sort((a, b) => {
    if (a.score > b.score) {
      return -1;
    }

    if (a.score < b.score) {
      return 1;
    }

    return 0;
  });
}

/** Calculates score for strings to be similar on the scale of 0 to 1 */
export function scoreByNameSimilarity(
  cities: ICityRawData[],
  searchString: string,
): IGetCitySuggestion[] {
  const suggestedCities: IGetCitySuggestion[] = [];
  cities.forEach((c) => {
    const specialCharsRegex = /[-.'\s]/g;
    const cityNameSanitized = c.ascii.replace(specialCharsRegex, '');
    const searchStringSanitized = searchString.replace(specialCharsRegex, '');

    const similarityScore = distance(cityNameSanitized, searchStringSanitized, {
      caseSensitive: false,
    });
    if (similarityScore > SCORE_THRESHOLD) {
      suggestedCities.push({
        name: getCityDetailString(c),
        latitude: c.latitude,
        longitude: c.longitude,
        score: Number(similarityScore.toFixed(1)),
      });
    }
  });
  return suggestedCities;
}
