import LRU from "lru-cache";

/**
 * LRUCacheService: LRU (Least Recently Used): It's a caching paradigm to evict data from the cache that has not been recently used, leaving only the cities that are accessed frequently
 * This will later be used for scoring to give priority to most searched items.
 */
export default class LRUCacheService {
  options = {
    max: 500,
    maxSize: 5000,
    sizeCalculation: (value, key) => {
      return 1;
    },
  };

  lruCache = new LRU(this.options);

  /**
   *
   * @param {string} key
   * @returns boolean: whether or not the item is present in cached.
   */
  isRecentlyUsed = (key) => {
    const isRU = this.lruCache.get(key);
    return isRU !== null && isRU !== undefined;
  };

  /**
   *
   * @param {string} key
   * @param {any} value
   */
  setItem = (key, value) => {
    if (!this.isRecentlyUsed(key)) {
      this.lruCache.set(key, value);
    }
  };
}
