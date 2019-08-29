/**
 * Shared queries cache
 */

const { LONGEST_CITY_NAME } = require('./data-loader');

const getCacheKey =
  /**
   * @param {string} str
   *
   */
  str =>
    str
      .trim()
      .normalize('NFKD')
      .substr(0, LONGEST_CITY_NAME)
      .toLowerCase();
module.exports.getCacheKey = getCacheKey;

const BadQueriesCache = /** @type {Set<string>} */ (new Set());
module.exports.BadQueriesCache = BadQueriesCache;
