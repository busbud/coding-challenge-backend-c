import { stringSimilarity } from 'string-similarity-js';
import { distance, point } from '@turf/turf';

import redisClient from '../modules/redis';

import type { Location } from '@prisma/client';

type Suggestion = Omit<
  Location,
  'ascii' | 'id' | 'createdAt' | 'updatedAt' | 'population'
>;

type ScoredSuggestion = Omit<Suggestion, 'country' | 'state'> & {
  score: number;
};

type TransformationFunction = (suggestion: Suggestion) => ScoredSuggestion;

const scoreAndSort = (
  transformFn: TransformationFunction,
  suggestions: Suggestion[],
) => suggestions.map(transformFn).sort((a, b) => b.score - a.score);

const getScoresBasedOnSearchTerm = (suggestions: Suggestion[], term: string) =>
  scoreAndSort(
    ({ country, latitude, longitude, name, state }) => ({
      name: `${name}, ${state}, ${country}`,
      latitude,
      longitude,
      score: parseFloat(stringSimilarity(term, name).toFixed(1)),
    }),
    suggestions,
  );

const getScoresBasedOnSearchTermAndLocation = (
  lat: number,
  long: number,
  suggestions: Suggestion[],
  term: string,
) =>
  scoreAndSort(({ country, latitude, longitude, name, state }) => {
    const from = point([lat, long]);
    const to = point([latitude, longitude]);
    const haversineDistance = distance(from, to);
    const haversineScore = 1 / (1 + haversineDistance); // normalizing the Haversine calculation
    const sorensenDiceScore = stringSimilarity(term, name);

    return {
      name: `${name}, ${state}, ${country}`,
      latitude,
      longitude,
      score: parseFloat(((haversineScore + sorensenDiceScore) / 2).toFixed(1)), // both should be weighted equally
    };
  }, suggestions);

const getRedisClientCache = async (
  term: string,
  latitude?: number,
  longitude?: number,
) => {
  let cacheResults: string | null;

  if (latitude && longitude) {
    cacheResults = await redisClient.get(`${term},${latitude},${longitude}`);
  } else {
    cacheResults = await redisClient.get(term);
  }

  return cacheResults;
};

export {
  getScoresBasedOnSearchTerm,
  getScoresBasedOnSearchTermAndLocation,
  getRedisClientCache,
};
export type { ScoredSuggestion };
