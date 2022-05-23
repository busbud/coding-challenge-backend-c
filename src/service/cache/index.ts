import cacheConfig from 'config/cache';

export interface CacheAdapter {
  get(keyPrefix: string, keyObject: any): Promise<string | null>
  set(keyPrefix: string, keyObject: any, expireTime: number, value: any): Promise<void>
  getSet(
    keyPrefix: string, keyObject: any, expireTime: number, value: () => Promise<any>
  ): Promise<string>
  getObject(keyPrefix: string, keyObject: any): Promise<any | null>
  setObject(keyPrefix: string, keyObject: any, expireTime: number, value: any): Promise<void>
  getSetObject(
    keyPrefix: string, keyObject: any, expireTime: number, value: () => Promise<any>
  ): Promise<any>
  delete(pattern: string): Promise<number>
  disconnect(): Promise<void>
}

export default new cacheConfig.AdapterClass();
