import ScoringService from "./scoring-service.js";
import cache from "memory-cache";
import { CACHE_KEYS } from "../utils/constants.js";
import Trie from "./trie/trie.js";

/**
 * Suggestions: this
 */
export default class SuggestionsService {
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
    this.indexedData = await this.index(data);

    cache.put(CACHE_KEYS, this.indexedData);
  };

  /**
   *
   * @param {Array} data
   * @returns Promise<Trie>
   */
  index = (data) => {
    return new Promise((resolve, reject) => {
      if (!data) reject("no data to index");
      if (!data.length) reject("no data to index");

      // initialize the tree
      const trie = new Trie();

      data.forEach((cityInformation) => {
        trie.insert(cityInformation.ascii, cityInformation);

        // Index the alternate names. Remove all special characters to only account for English alphabet words
        const altNames = cityInformation.alt_name
          ?.split(",")
          .map((r) => r.replace(/[^a-zA-Z ]/g, " ").trim())
          .filter((p) => p);

        if (altNames?.length) {
          altNames.forEach((alt) => {
            trie.insert(alt, cityInformation);
          });
        }
      });

      resolve(trie);
    });
  };

  /**
   *
   * @param {string} term Search term
   * @param {number} lat
   * @param {number} long
   * @returns
   */

  searchData = async (term, lat, long) => {
    if (!term) return [];
    if (!term.length) return [];
    if (term.length < 3) return [];

    const cachedResult = cache.get(term);
    let results = [];

    if (cachedResult) {
      results = cachedResult;
    } else {
      results = await this.indexedData.search(term);
    }

    if (results.length && results.length >= 50) {
      cache.put(term, results);
    }

    const scoreAndSort = new ScoringService(this.lruCache);
    const scoredAndSorted = scoreAndSort.score(results, lat, long);

    if (scoredAndSorted.length === 1) {
      if (!this.lruCache.isRecentlyUsed(scoredAndSorted[0].name)) {
        this.lruCache.setItem(scoredAndSorted[0].name, 1);
      }
    }

    return scoredAndSorted;
  };
}
