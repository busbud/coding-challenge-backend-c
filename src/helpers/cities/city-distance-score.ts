import env from '../../config/env';
import { Location } from '../../domain/models';
import { calculateDistanceInKm } from '../utils/distance';

export function calculateDistanceScore(
  location1: Location,
  location2: Location
) {
  const { latitude: latitude1, longitude: longitude1 } = location1;
  const { latitude: latitude2, longitude: longitude2 } = location2;

  const distance = calculateDistanceInKm(
    latitude1,
    longitude1,
    latitude2,
    longitude2
  );

  const score = 1 - Math.min(distance / env.cities.maxDistanceAddScore, 1);

  return score;
}

type CityWithLocationAndScore<T> = T &
  Location & {
    score: number;
  };

export function addCityLocationScore<T>(
  city: CityWithLocationAndScore<T>,
  location: Location
) {
  const nameScore = city.score * (1 - env.cities.distanceScorePercentage);
  const distanceScore =
    calculateDistanceScore(city, location) * env.cities.distanceScorePercentage;

  const score = nameScore + distanceScore;

  return {
    ...city,
    score,
  };
}
