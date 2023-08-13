import { SetOptions } from 'redis';
import { RedisClient } from '../../../config/redis';
import { CitySuggestion } from '../../../domain/models';
import { buildCitiesSuggestionsKey } from './helper';

export async function setCitiesSuggestionsToCache(
  redis: RedisClient,
  query: string,
  cities: CitySuggestion[],
  options?: SetOptions
) {
  await redis.set(
    buildCitiesSuggestionsKey(query),
    JSON.stringify(cities),
    options
  );
}
