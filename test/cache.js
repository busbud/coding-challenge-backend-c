const expect  = require('chai').expect;
const Cache = require('../src/utils/cache');

describe('Test cache system', function() {
  describe('with specific key', function () {
    
    var cache = new Cache(1, '#');

    var key = cache.getCacheKey("test", 10, 1, 1);

    it('returns correct key', function () {
      expect(key).to.equal("test#10#1#1");
    });

  });

  describe('with search result to cache', function() {
    
    var cache = new Cache(5, '#');
    var key = cache.getCacheKey("test", 10, 1, 1);
    var key2 = cache.getCacheKey("test2", 10, 1, 1);
    var results = { "test": 1 };
    var results2 = { "test": 2 };
    cache.addSearchToCache(key, results);
    cache.addSearchToCache(key2, results2);

    it('returns cache results', function() {
      expect(cache.getSearchFromCache(key)).to.eql({ "test": 1 });
      expect(cache.getSearchFromCache(key2)).to.eql({ "test": 2 });
    });
  });


  describe('with search result to limited cache', function() {
    
    var cache = new Cache(1, '#');
    var key = cache.getCacheKey("test", 10, 1, 1);
    var key2 = cache.getCacheKey("test2", 10, 1, 1);
    var results = { "test": 1 };
    var results2 = { "test": 2 };
    cache.addSearchToCache(key, results);
    cache.addSearchToCache(key2, results2);

    it('returns empty cache with only last result', function() {
      expect(cache.getSearchFromCache(key)).to.equal(undefined);
      expect(cache.getSearchFromCache(key2)).to.eql({ "test": 2 });
    });
  });


});