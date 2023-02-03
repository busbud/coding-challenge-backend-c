import { GeoPosition } from 'geo-position.ts';
import {ICityRawData, IGetCitySuggestion} from '../interfaces/interfaces';
import {getCityDetailString} from "./geography.util";
/* eslint @typescript-eslint/no-var-requires: "off" */
const distance = require('jaro-winkler');

/** Calculates score for distance range on the scale of 0 to 1 */
function calculateScoreForDistance(distances: number[]) {
  const min = Math.min(...distances);
  const max = Math.max(...distances);
  const range = max - min;
  return distances.map((dis) => {
    return 1 - (dis - min) / range;
  });
}

/** Calculates score by factoring in string similarity and  distance score on the scale of 0 to 1 */
function updateScoreBasedOnDistance(citySuggestions: IGetCitySuggestion[], scores: number[]) {
  const distanceWeight = 0.5;
  const spellingWeight = 0.5;
  return citySuggestions.map((suggestion, idx) => {
    const score = suggestion.score * spellingWeight + scores[idx] * distanceWeight;
    return {
      ...suggestion,
      score: Number(score.toFixed(1))
    };
  });
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
  return cities.sort((a, b) => (a.score >= b.score ? 1 : 0));
}

/** Calculates score for strings to be similar on the scale of 0 to 1 */
export function scoreByNameSimilarity(cities: ICityRawData[], searchString: string): IGetCitySuggestion[] {
  const suggestedCities: IGetCitySuggestion[] = [];
  cities.forEach((c) => {
    if (!c.ascii.toLowerCase().startsWith(searchString.toLowerCase())) {
      return;
    }
    const similarityScore = distance(searchString, c.ascii, { caseSensitive: false });
    if (similarityScore > 0.33) {
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