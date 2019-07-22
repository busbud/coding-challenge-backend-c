// General Libraries
const log4js            = require('log4js');
const dataImporter      = require("./dataImporter")
// Application Code
const suggestionConfig  = require('../config').suggestionConfig;
const admin1Code        = require('../data/admin_1_code');

// Configure Logging
const logger            =  log4js.getLogger();
logger.level            =  'debug';

/**
 * Filters out a city based on the suggestion configuration
 * Perform the filters here so that it only needs to peform this filter on the server load
 * @param   {Object}  city              The city
 * @param   {number}  city.population   The population of a city
 * @param   {string}  city.country      The ISOCODE country of the City
 * @return  {boolean} is_valid_city     Returns true when city passes suggestiosn configuration criterion
 */
function strictCityFilter(city){
  var is_valid_city  = true
  if(suggestionConfig.minPopulation) {
    is_valid_city = is_valid_city  && (city.population >= suggestionConfig.minPopulation)
  }
  if(suggestionConfig.maxPopulation) {
    is_valid_city = is_valid_city  && (city.population <= suggestionConfig.minPopulation)
  }
  if(suggestionConfig.countryWhitelist) {
    is_valid_city = is_valid_city  && (suggestionConfig.countryWhitelist.includes(city.country))
  }
  return is_valid_city;
};

/**
 * Filters out a city based on the suggestion configuration
 * Perform the filters here so that it only needs to peform this filter on the server load
 * @param   {Object}  city              The city
 * @param   {number}  city.population   The population of a city
 * @param   {string}  city.lat          The latitude of the city
 * @param   {string}  city.long         The longitude of the city
 * @param   {string}  city.admin1       The admin1 code of the city
 * @return  {Object}  city              The formatted city
 */
function cityFormatter(city){
  city.population = parseInt(city.population);
  city.coordinate = {
    latitude: parseFloat(city.lat),
    longitude: parseFloat(city.long)
  };
  let stateKey = [city.country,city.admin1].join(".");
  let state = admin1Code[stateKey];
  city.state = (state ? state.isocode2 : city.admin1);
  return city;
}

/**
 * Variable used to store imported city data
 */
let data  = [];

/**
 * Filters out a city based on the suggestion configuration
 * Perform the filters here so that it only needs to peform this filter on the server load
 * @param   {string}    path            The path containing the data to be imported
 * @param   {string}    lineDelimiter   Character that delimates a new line
 * @param   {function}  callback        function called once data is imported
 * @return  {void}
 */
async function importData(path, lineDelimiter, callback){
  logger.info(`Importing data: ${path}`);
  await dataImporter(path, 'utf8', true, lineDelimiter, cityFormatter, strictCityFilter)
    .then(function(imported_data){
      // stores imported_data into module variable
      data = imported_data;
      logger.info(`${data.length} data rows have been imported`);
    }).catch(function(error){
      logger.error(`${error.name} - ${error.message}`);
    });
  if(callback){
    callback();
  }
}

module.exports = {
  importData: importData,
  getData: () => data,
};

