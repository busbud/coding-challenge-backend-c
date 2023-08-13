import { PrismaClient } from '@prisma/client';
import env from '../../config/env';
import { CitySuggestion, Location } from '../../domain/models';
import { findManyCitiesSuggestions } from '../../infra/db/cities/find-many-cities-suggestions';
import { addCityLocationScore } from '../../helpers/cities/city-distance-score';

const DEFAULT_LIMIT = 10;

export async function getCitiesSuggestions(
  prisma: PrismaClient,
  name: string,
  location?: Location,
  limit?: number
): Promise<CitySuggestion[]> {
  const cities = await findManyCitiesSuggestions(prisma, {
    name,
    minimumPopulation: env.cities.largeCitiesMinimumPopulation,
    countryCodes: env.cities.acceptedCountryCodes,
    limit: limit || DEFAULT_LIMIT,
  });

  if (!location) {
    return cities;
  }

  const citiesWithDistanceScore = cities
    .map((city) => addCityLocationScore(city, location))
    .sort((a, b) => (a.score > b.score ? -1 : 1));

  return citiesWithDistanceScore;
}
