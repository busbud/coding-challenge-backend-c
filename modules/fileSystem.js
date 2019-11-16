const config = require('./global.js');
const fs = require('fs');

const citiesString = fs.readFileSync(config.TSV_PATH, { encoding: 'utf-8' });

/**
 * Returns the cities array from file tsv
 *
 * @returns [{city}]
 */
let getCities = () => {
  return citiesString
    .split('\n')
    .slice(1) // first line is the headers, so we drop it
    .map((line) => line.split('\t')) // tab is the delimiter of our string
    .map(
      ([
        _geo_id,
        name,
        asciiname,
        alternatenames,
        latitude,
        longitude,
        _featureClass,
        _fcode,
        countryCode,
        countryCode2,
        adminCode1,
        _adminCode2,
        _adminCode3,
        _adminCode4,
        population,
        _elevation,
        _dem,
        _tz,
        _modified_at,
      ]) => ({
        name,
        asciiname,
        alternatenames,
        latitude,
        longitude,
        countryCode,
        adminCode1,
        population,
      })
    )
    .filter(
      (
        city // filter to valid our data
      ) => city.name && city.asciiname
    );
};

const cities = getCities();
/**
 *Given a location name, it returns the matching locations From the array of cities
 * the number of inhabitants musy be >=5k
 *
 * @param String
 * @returns []
 */
module.exports.getMatchingCities = (
  locationName,
  population = config.MAX_POPULATION
) => {
  const normalizeLocationName = normalizeString(locationName);
  return cities.filter(
    (city) =>
      normalizeString(city.asciiname).indexOf(normalizeLocationName) == 0 &&
      parseFloat(city.population) >= population
  );
};

/**
 * Removes accents and diatrics of a string
 * https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
 *
 * @param {String} string
 * @returns {String}
 */
const normalizeString = (string) =>
  string
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase();
