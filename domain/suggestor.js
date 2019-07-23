// Node Lib
const { promisify } = require('util');
// Application Sepcific
const log4js = require('log4js');
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
 * Adds city score attribute
 * @param   {string}  search_term                  Search term
 * @param   {Object}  search_coordinate            Search Coordinate
 * @param   {number}  search_coordinate.latitude   Search Coordinate's Latitude
 * @param   {number}  search_coordinate.longitude  Search Coordinate's Longitude
 * @return  {Array}   cities                       Array of cities sorted by score and filtered by search query
 */
async function suggestor(search_term, search_coordinate) {
  // create cache key based on normatlize search term
  const cache_key = cleanAndNormalizeString(search_term);
  // retrieve cached suggestions
  const cached_suggestions = await getAsync(cache_key);
  var suggestions = [];
  if(cached_list){
    // cache hit found a suggested list
    suggestions = JSON.parse(cached_list);
  } else {
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
  if(!cached_list){
    client.set(cache_key,JSON.stringify(suggestions));
  }
  return cities

}

module.exports = {
  getSuggestions: suggestor
};
