import cacheConfig from 'config/cache';
import BypassCache from 'service/cache/bypass';
import RedisCache from 'service/cache/redis';

export interface CacheInterface {
  getObject(key: string): Promise<any>
  setObject(key: string, object: any, expireTime: number): Promise<void>
  getSetObject(key: string, expireTime: number, freshObject: () => Promise<any>): Promise<any>
  delete(pattern: string): Promise<number>
  disconnect(): Promise<void>
}

const cache: CacheInterface = cacheConfig.bypass
  ? new BypassCache()
  : new RedisCache();

export default cache;
