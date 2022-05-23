import redisConfig from 'config/redis';
import RedisCache from 'service/cache/redis';

jest.mock('config/redis');

jest.mock('ioredis', () => ({
  __esModule: true,
  default: class MonoCache {
    private key = '';

    private value = '';

    async get(key: string) {
      return this.key !== '' && this.key === key
        ? this.value
        : null;
    }

    async set(key: string, value: string) {
      this.key = key;
      this.value = value;
    }

    async keys(pattern: string) {
      return this.key !== '' && pattern === (this.key.substring(0, pattern.length))
        ? [this.key]
        : [];
    }

    async del(keys: string[]) {
      if (keys.length && keys[0] === this.key) {
        this.key = '';
        this.value = '';
      }
    }

    async disconnect() {
      await this.del(await this.keys(''));
    }
  },
}));

let cache: RedisCache;

describe('constructor()', () => {
  describe('Using connection parameters', () => {
    test('Returns instance of RedisCache', async () => {
      cache = new RedisCache();
      expect(cache).toBeInstanceOf(RedisCache);
    });
  });
  describe('Using a connection URL', () => {
    test('Returns instance of RedisCache', async () => {
      redisConfig.url = 'someUrl';
      cache = new RedisCache();
      expect(cache).toBeInstanceOf(RedisCache);
    });
  });
});

describe('getSet()', () => {
  describe('With a non-existent cache key', () => {
    test('Returns input value', async () => {
      expect(await cache.getSet('someCacheKey', {}, 1, async () => 'someValue')).toBe('someValue');
    });
  });
  describe('With an existent cache key', () => {
    test('Returns stored value', async () => {
      expect(await cache.getSet('someCacheKey', {}, 1, async () => 'someOtherValue')).toBe('someValue');
    });
  });
});

describe('getSetObject()', () => {
  describe('With a non-existent cache key', () => {
    test('Returns input value', async () => {
      expect(await cache.getSetObject('someObjectCacheKey', {}, 1, async () => ({ value: 'someValue' }))).toMatchObject({ value: 'someValue' });
    });
  });
  describe('With an existent cache key', () => {
    test('Returns stored value', async () => {
      expect(await cache.getSetObject('someObjectCacheKey', {}, 1, async () => ({ value: 'someOtherValue' }))).toMatchObject({ value: 'someValue' });
    });
  });
});

describe('delete()', () => {
  describe('With a non-matching pattern', () => {
    test('Returns 0', async () => {
      expect(await cache.delete('someCacheKey')).toBe(0);
    });
  });
  describe('With a matching pattern', () => {
    test('Returns a positive number', async () => {
      expect(await cache.delete('someObjectCacheKey')).toBeGreaterThanOrEqual(1);
    });
  });
});

describe('disconnect()', () => {
  test('Returns void', async () => {
    expect(await cache.disconnect()).toBe(undefined);
  });
});
