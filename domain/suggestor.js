// Node Lib
const { promisify } = require('util');
// Application Sepcific
const log4js = require('log4js');
const cachingConfig = require('../config').caching;
const { getData } = require('../lib/loadData');
const { searchString, scoreCity, cleanAndNormalizeString } = require('./suggestor.helper.js');
const client = require('../lib/configureRedis');
const getAsync = promisify(client.get).bind(client);
const logger = log4js.getLogger();
logger.level = 'debug';

/**
 * Adds city score attribute
 * @param   {Object}  city                        City Object
 * @param   {string}  search_term                 Query string
 * @param   {Object}  search_coordinate           Search Coordinate
 * @param   {number}  search_coordinate.latitude  Search Coordinate's Latitude
 * @param   {number}  search_coordinate.longitude Search Coordinate's Longitude
 * @return  {Object}  city                        City object with score attribute added
 * @return  {number}  city.score                  City score
 */
function getScoredCity(city, search_term, search_coordinate) {
  return {
    ...city,
    score: scoreCity(city, search_term, search_coordinate)
  };
}

/**
 * Returns cached suggestion set or null if caching is disabled or cache misses
 * @param   {string}  search_term   Query string
 * @return  {Array}   results       Cache results
 */

async function getCacheRequest(search_term){

  if(cached_suggestions){

  }else {}
    suggestions = JSON.parse(cached_suggestions);
  return cached_suggestions
}

/**
 * Returns a formatted & filtered list of suggested cities based on search terms
 * @param   {string}  search_term                  Search term
 * @param   {Object}  search_coordinate            Search Coordinate
 * @param   {number}  search_coordinate.latitude   Search Coordinate's Latitude
 * @param   {number}  search_coordinate.longitude  Search Coordinate's Longitude
 * @return  {Array}   cities                       Array of cities sorted by score and filtered by search query
 */
async function suggestor(search_term, search_coordinate) {
  var cache_key = null;
  var cached_suggestions = null;
  if(cachingConfig.enable){
    // create cache key based on normalize search term
    cache_key = cleanAndNormalizeString(search_term);
    logger.info('checking cache at key ',cache_key);
    // retrieve cached suggestions when redis is ready
    cached_suggestions = await getAsync(cache_key);
  }
  var suggestions = [];
  if (cached_suggestions) {
    // cache hit found a suggested list
    logger.info('cache hit at key ',cache_key);
    suggestions = JSON.parse(cached_suggestions);
  } else {
    logger.info('cache miss at key ',cache_key);
    // cache hit found a suggested list
    suggestions = getData();
    suggestions = suggestions.filter(function(city) {
      // search by string
      const searchState = searchString(city.ascii, search_term);
      return searchState.found;
    });
  }
  suggestions = suggestions.map(function(city) {
    // score city
    return getScoredCity(city, search_term, search_coordinate);
  }).sort(function(city_a, city_b) {
    // sort cities
    return (city_b.score - city_a.score);
  });
  if (cache_key && !cached_suggestions) {
    // set cache to expire based on config file or default to 24 hours
    client.set(cache_key, JSON.stringify(suggestions), 'EX', (cachingConfig.expiry || (60 * 60 * 24)));
  }
  return suggestions;
}

module.exports = {
  getSuggestions: suggestor
};
