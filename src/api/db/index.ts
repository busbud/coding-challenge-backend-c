import { createClient, RedisClient } from 'redis';
import { REDIS_URL } from 'api/config';

export type Connection = RedisClient;

export async function connect() {
  return createClient({ url: REDIS_URL });
}
