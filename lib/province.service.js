const LRU = require('lru');

class ProvinceService {
  constructor(mongoClient) {
    this.db = mongoClient.db();
    this.cache = new LRU(100); // 100 items max in lru cache
  }

  async getProvinceByCode(code) {
    let result = this.cache.get(code);
    if (!result) {
      result = await this.db.collection('provinces').findOne({ code });
      this.cache.set(code, result);
    }
    return result;
  }
}

module.exports = ProvinceService;
