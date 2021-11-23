/**
 * Basic Memory Cache Class based on Timothy Barmann's code
 * 
 * Caching the entire data set isn't a good idea. It can generate serious memory problems in a production environment.
 * This simple cache is here as a proof of concept, as caching is a way to reduce server load during high traffic.
 * 
 */
class MemoryCache {

  constructor(callback, cacheInvalidationTime = 10) {
    this.cacheInvalidationTimeInMilliseconds = cacheInvalidationTime * 60 * 1000;
    this.callback = callback;
    this.cache = null;
    this.getData = this.getData.bind(this);
    this.invalidate = this.invalidate.bind(this);
    this.isValid = this.isValid.bind(this);
    this.lastUpdate = new Date(0);
  }

  isValid() {
    return (this.lastUpdate.getTime() + this.cacheInvalidationTimeInMilliseconds) > new Date().getTime();
  }

  getData(...params) {
    if (!this.cache || !this.isValid()) {
      return this.callback(...params).then((data) => {
        this.cache = data;
        this.lastUpdate = new Date();

        return data;
      });
    } else {
      return Promise.resolve(this.cache);
    }
  }

  invalidate() {
    this.lastUpdate = new Date(0);
  }
}

exports.MemoryCache = MemoryCache;