import env from '../../config/env';
import { Context } from '../../types/express';
import { CitySuggestion, Location } from '../../domain/models';
import { findManyCitiesSuggestions } from '../../infra/db/cities/find-many-cities-suggestions';
import { addCityLocationScore } from '../../helpers/cities/city-distance-score';
import {
  getCitiesSuggestionsFromCache,
  setCitiesSuggestionsToCache,
} from '../../infra/cache/cities';

const DEFAULT_LIMIT = 10;
const DEFAULT_CACHE_EXPIRE_SECONDS = 60;

export async function getCitiesSuggestions(
  context: Context,
  name: string,
  location?: Location,
  limit?: number
): Promise<CitySuggestion[]> {
  const { prisma, redis } = context;

  let cities: CitySuggestion[] | null | undefined;

  cities = await getCitiesSuggestionsFromCache(redis, name);

  if (!cities) {
    cities = await findManyCitiesSuggestions(prisma, {
      name,
      minimumPopulation: env.cities.largeCitiesMinimumPopulation,
      countryCodes: env.cities.acceptedCountryCodes,
      limit: limit || DEFAULT_LIMIT,
    });

    setCitiesSuggestionsToCache(redis, name, cities, {
      EX: DEFAULT_CACHE_EXPIRE_SECONDS,
    });
  }

  if (!location) {
    return cities;
  }

  const citiesWithDistanceScore = cities
    .map((city) => addCityLocationScore(city, location))
    .sort((a, b) => (a.score > b.score ? -1 : 1));

  return citiesWithDistanceScore;
}
