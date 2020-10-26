const CACHE_LIFETIME = 1000;
const sha1 = require('sha1');
const validator = require('validator');
const cache = require('memory-cache');
// const RadixTrie = require("radix-trie-js");
const TrieSearch = require('trie-search');
const latinize = require('latinize');
const utf8 = require('utf8');

const trie = new TrieSearch(); // used to load and search data using trie ds
const scoreCalculator = require('./calculateScore');

const getSuggestionsAndPonderate = async (q, latitude, longitude) => {
  var uniq = {};
  let result = trie.get(q); // finds all elements of the trie that match
  if (result.length) {
    // remove possible duplicates as trie may have same element indexed on different keys
    const arrFiltered = result.filter(obj => !uniq[obj.value.id] && (uniq[obj.value.id] = true));
    // calculates score on possible results
    result = scoreCalculator.calculateScoreForAllAndSort(arrFiltered, q, latitude, longitude);
  }
  return result;
};

function getSanitizedParamsFromQuery(query) {
  const params = {};
  if ('q' in query && typeof query.q === 'string' && query.q.length > 0) {
    // removed validator.isAlpha(query.q) because it's not working with spaces. TODO: must verify.
    let qry = query.q;
    try {
      // first try to decode if possible. then latinize.
      // this way it works for both q=Montréal and q=Montr%C3%A9al
      qry = utf8.decode(query.q);
    } catch (e) {
      // ignore possible exception when decoding
    }
    // In order to simplify searching, ignore uppercase and blank spaces
    // People searching for "newyork" are probably looking for "New York"
    // This exceeds "sanitization" and is actually "normalization" for search.
    // Since there is only one endpoint and one user of the trie, it's not so bad to have it here.
    params.q = latinize(qry).toLowerCase().replace(/ /g, '');
  } else {
    // should this API allow getting results without any query param?
    return null;
  }
  /* Location precision
   * Location data is usually very imprecise and thus very erratic.
   * Same user may report several locations within a 50m radius.
   * Also, given distance between cities, sending the same suggestions to
   * two distinct users who are 100m apart seams perfectly reasonable.
   * A very quick way to do this is to simply truncate decimals
   * see https://en.wikipedia.org/wiki/Decimal_degrees
   * 3 decimals gives precision close to 100m
   * The reason for doing this is mostly to increase cache hit probability
   */
  if ('longitude' in query) {
    params.longitude = parseFloat(query.longitude).toFixed(3); //TODO: This magic number should be configurable.
  }
  if ('latitude' in query) {
    params.latitude = parseFloat(query.latitude).toFixed(3);
  }
  return params;
}

/* Cache
 * Very simpe caching strategy
 * Store requests during CACHE_LIFETIME in cache memory
 * If attempting to get an element that exists in cache, return stored value
 * If queried element is not in cache, process it and store it
 * Strategy of storing for X time is very very simple and only for demonstration purposes
 * Real-life caches should of course implement a limit of stored elements.
 * This one may grow indefinitively.
 * Then again, real-life HTTP caching would probably be done in
 * another layer (gateway or proxy) and not in this app.
 *
 * As said, this cache here is just a very simple strategy for a very simple demo.
 */
const getFromCacheOrProcess = async (req, res) => {
  const params = getSanitizedParamsFromQuery(req.query);
  if (!params) {
    return null;
  }

  const result = {};
  const hash = sha1(`q${params.q}n${params.longitude}t${params.latitude}`);
  const cachedResult = cache.get(hash);
  if (cachedResult) {
    result.data = cachedResult.data;
    result.time = cachedResult.time;
  } else {
    const data = await getSuggestionsAndPonderate(params.q, params.latitude, params.longitude);
    cache.put(hash, { data, time: Date.now() }, CACHE_LIFETIME);
    result.data = data;
    result.time = null;
  }
  return result;
};

exports.trie = trie;
exports.getFromCacheOrProcess = getFromCacheOrProcess;
