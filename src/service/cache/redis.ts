import IORedis, { Redis } from 'ioredis';
import hashObject from 'object-hash';
import { CacheAdapter } from 'service/cache';
import redisConfig from 'config/redis';

export default class RedisCache implements CacheAdapter {
  private redis: Redis;

  private readonly keySeparator = ':';

  constructor() {
    this.redis = redisConfig.url
      ? new IORedis(redisConfig.url, redisConfig)
      : new IORedis(redisConfig);
  }

  private key(keyPrefix: string, keyObject: any) {
    return `${keyPrefix}${this.keySeparator}${hashObject(keyObject)}`;
  }

  async get(keyPrefix: string, keyObject: any) {
    return this.redis.get(this.key(keyPrefix, keyObject));
  }

  async set(keyPrefix: string, keyObject: any, expireTime: number, value: string) {
    await this.redis.set(this.key(keyPrefix, keyObject), value, 'EX', expireTime);
  }

  async getSet(
    keyPrefix: string,
    keyObject: any,
    expireTime: number,
    value: () => Promise<string>,
  ) {
    let v = await this.get(keyPrefix, keyObject);
    if (v === null) {
      v = await value();
      await this.set(keyPrefix, keyObject, expireTime, v);
    }
    return v;
  }

  async getObject(keyPrefix: string, keyObject: any) {
    const value = await this.get(keyPrefix, keyObject);
    return value === null ? null : JSON.parse(value);
  }

  async setObject(keyPrefix: string, keyObject: any, expireTime: number, value: any) {
    await this.set(keyPrefix, keyObject, expireTime, JSON.stringify(value));
  }

  async getSetObject(
    keyPrefix: string,
    keyObject: any,
    expireTime: number,
    value: () => Promise<any>,
  ) {
    let v = await this.getObject(keyPrefix, keyObject);
    if (v === null) {
      v = await value();
      await this.setObject(keyPrefix, keyObject, expireTime, v);
    }
    return v;
  }

  async delete(pattern: string) {
    let batch: string[] = [];
    const keys = await this.redis.keys(pattern);
    const count = keys.length;
    do {
      batch = keys.splice(0, 1000);
      if (batch.length) {
        await this.redis.del(batch);
      }
    } while (keys.length);
    return count;
  }

  async disconnect() {
    this.redis.disconnect();
  }
}
