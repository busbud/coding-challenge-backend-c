/**
 * Requests caching logic
 *
 * I use in-memory caching in this challenge, but in real production something like Redis must be used
 * to allow horizontal scaling
 */

'use strict';

const { getCacheKey, BadQueriesCache } = require('./cache');

/**
 * @todo In real production we may want to limit supported locales here
 */
const collator = new Intl.Collator([], {
  usage: 'search',
  sensitivity: 'base',
  ignorePunctuation: true
});

/**
 * Checks if we can respond to this request either use information from session, bad queries or previous queries caches
 * Currently it caches only bad queries with no suggestions
 * but can be easily extended to cache whole results with LRU cache
 *
 * @param {import('koa').Context} ctx
 * @returns {Promise<boolean>} - if returns true following request processing must be stopped
 */
async function isQueryCached(ctx) {
  // Check if we have a suggestion stored in current request session and query doesn't changed
  if (collator.compare(ctx.session.lastQuery, ctx.query.q) === 0) {
    ctx.body = ctx.session.lastSuggestions;
    ctx.status = 304; // not modified
    return true;
  }
  ctx.session.lastQuery = ctx.query.q;
  ctx.session.lastSuggestions = [];

  // Normalize query to cache key by stripping accents, lowercase and shorten till LONGEST_CITY_NAME
  const cacheKey = getCacheKey(ctx.query.q);
  // check bad requests cache
  if (BadQueriesCache.has(cacheKey)) {
    ctx.body = { suggestions: [] };
    ctx.status = 404; // Really bad API design from my point of view, but required by tests
    return true;
  }

  return false;
}
module.exports.isQueryCached = isQueryCached;

/**
 * Stores results in cache
 * Currently only saves 'bad' queries, resulted in no suggestions
 * but can be easily extended to cache whole query should any performance bottleneck occur
 *
 * @param {import('koa').Context} ctx
 */
async function cacheResults(ctx) {
  const cacheKey = getCacheKey(ctx.query.q);
  if (ctx.body.suggestions.length < 1) {
    BadQueriesCache.add(cacheKey);
    ctx.status = 404;
  }
}
module.exports.cacheResults = cacheResults;
