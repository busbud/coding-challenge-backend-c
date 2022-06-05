import IndexingService from "./indexing-service.js";
import ScoringService from "./scoring-service.js";
import cache from "memory-cache";
import { CACHE_KEYS } from "../utils/constants.js";

class Suggestions {
  indexedData = null;
  constructor(datasource, lruCache) {
    this.datasource = datasource;
    this.lruCache = lruCache;
  }

  initialize = async () => {
    const cached = cache.get(CACHE_KEYS);
    if (cached) {
      this.indexedData = cached;
      return;
    }
    const data = await this.datasource.getData();
    const indexService = new IndexingService(data);
    this.indexedData = await indexService.index();
    cache.put(CACHE_KEYS, this.indexedData);
  };

  searchData = async (term, lat, long) => {
    if (!term) return [];
    if (!term.length) return [];

    const results = await this.indexedData.search(term);
    const scoreAndSort = new ScoringService(this.lruCache);
    const scoredAndSorted = scoreAndSort.score(results, lat, long);

    return scoredAndSorted;
  };
}

export default Suggestions;
