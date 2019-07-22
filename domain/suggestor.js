// Application Sepcific
const { getData }       = require('../lib/loadData');
const {
  searchString,
  distanceBetweenCoordinates,
  scoreCity
}                       = require('./suggestor.helper.js');

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
function getScoredCity(city, search_term, search_coordinate){
  return {
    ...city,
    score: scoreCity(city ,search_term, search_coordinate)
  }
}


/**
 * Adds city score attribute
 * @param   {string}  search_term                  Search term
 * @param   {Object}  search_coordinate            Search Coordinate
 * @param   {number}  search_coordinate.latitude   Search Coordinate's Latitude
 * @param   {number}  search_coordinate.longitude  Search Coordinate's Longitude
 * @return  {Array}   cities                       Array of cities sorted by score and filtered by search query
 */
function suggestor(search_term, search_coordinate) {
  var cities = getData();
  return cities.filter(function(city) {
    // search by string
    let searchState = searchString(city.ascii,search_term);
    return searchState.found
  }).map(function(city) {
    // score city
    return getScoredCity(city, search_term, search_coordinate);
  }).sort(function(city_a, city_b){
    // sort cities
    return (city_b.score - city_a.score)
  });
}

module.exports = {
  getSuggestions: suggestor
};
