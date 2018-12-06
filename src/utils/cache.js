module.exports = 

/**
 * Cache for search results
 */
class Cache {

    constructor(maxSize, keyDelimiter) {
        this.maxSize = maxSize;
        this.keyDelimiter = keyDelimiter;
        this.cache = {};
        this.cacheIdx = 0;
    }

    isSearchInCache(key) {
        return key in this.cache;
    }
    
    getCacheKey(term, limit, lat, long) {
        return term+this.keyDelimiter+limit+this.keyDelimiter+lat+this.keyDelimiter+long;
    }
    
    getSearchFromCache(key) {
        return this.cache[key];
    }
    
    addSearchToCache(key, value) {
        if (this.cacheIdx >= this.maxSize) {
            this.cache = {};
            this.cacheIdx = 0;
        }
        this.cacheIdx++;
        this.cache[key] = value;
    }
    
}