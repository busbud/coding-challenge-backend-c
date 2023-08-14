import { Express } from 'express';
import { createClient, RedisClientType } from 'redis';
import env from './env';

const modules = {};
const scripts = {};

export type RedisClient = RedisClientType<typeof modules, typeof scripts>;

export async function createRedisClient() {
  const client = createClient({
    modules,
    scripts,
    url: env.cache.url,
    socket: {
      connectTimeout: env.cache.timeout,
    },
  });

  client.on('error', (err) => console.error('Redis Client Error', err));

  await client.connect();

  return client;
}

export async function setupRedisClient(app: Express) {
  const client = await createRedisClient();

  app.redis = client;
}
