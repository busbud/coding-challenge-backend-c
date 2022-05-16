import IORedis, { Redis } from 'ioredis';
import redisConfig from 'config/redis';
import { CacheInterface } from 'service/cache';

export default class RedisCache implements CacheInterface {
  private redis: Redis;

  constructor() {
    this.redis = redisConfig.url
      ? new IORedis(redisConfig.url, redisConfig)
      : new IORedis(redisConfig);
  }

  async getObject(key: string) {
    const object = await this.redis.get(key);
    if (object) {
      return JSON.parse(object);
    }
    return object;
  }

  async setObject(key: string, expireTime: number, object: any) {
    await this.redis.set(key, JSON.stringify(object), 'EX', expireTime);
  }

  async getSetObject(key: string, expireTime: number, freshObject: () => Promise<any>) {
    let object = await this.getObject(key);
    if (!object) {
      object = await freshObject();
      await this.setObject(key, expireTime, object);
    }
    return object;
  }

  async delete(pattern: string) {
    let deleted = 0;
    if (this.redis) {
      let batch: string[] = [];
      const keys = await this.redis.keys(pattern);
      do {
        batch = keys.splice(0, 1000);
        if (batch.length) {
          await this.redis.del(batch);
          deleted += batch.length;
        }
      } while (keys.length);
    }
    return deleted;
  }

  async disconnect() {
    this.redis.disconnect();
  }
}
