import { REDIS_URL } from 'api/config';
import redis, { Redis } from 'ioredis';

export type Connection = Redis;

export async function connect(): Promise<Connection> {
  return new redis(REDIS_URL);
}
