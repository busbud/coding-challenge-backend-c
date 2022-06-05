import LRU from "lru-cache";

export default class LRUCacheService {
  constructor() {}

  options = {
    max: 500,
    maxSize: 5000,
    sizeCalculation: (value, key) => {
      return 1;
    },
  };

  lruCache = new LRU(this.options);

  isRecentlyUsed = (key) => {
    const isRU = this.lruCache.get(key);
    return isRU !== null && isRU !== undefined;
  };

  setItem = (key, value) => {
    if (!this.isRecentlyUsed(key)) {
      this.lruCache.set(key, value);
    }
  };
}
