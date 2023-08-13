import { RedisClient } from '../../../config/redis';
import { CitySuggestion } from '../../../domain/models';
import { buildCitiesSuggestionsKey } from './helper';

export async function getCitiesSuggestionsFromCache(
  redis: RedisClient,
  query: string
): Promise<CitySuggestion[] | null> {
  const key = buildCitiesSuggestionsKey(query);
  const value = await redis.get(key);

  if (!value) {
    return null;
  }

  const cities = JSON.parse(value) as CitySuggestion[];
  return cities;
}
