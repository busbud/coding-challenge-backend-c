/**
 * Required Modules.
 */
const fs = require('fs');
const csv = require('csv-parser');

/**
 * Read and parse a .csv file.
 *
 * @param {Object} Object       Object used as an interface to get some properties.
 * @param {String} Object.file  The file to be parsed.
 * @return {Promise}            An array of objects as a promise.
 */
const readAndParseCSVFile = ({ file }) =>
  new Promise((resolve, reject) => {
    const response = [];

    // Create a read stream for the file <file>.
    return fs
      .createReadStream(file)
      .pipe(csv({}))
      .on('data', (data) => response.push(data))
      .on('error', (error) => reject(error))
      .on('end', () =>
        resolve(
          response.map((object) => ({
            ...object,
            population: Number(object.population), // Transform the value in order to compare it as a number.
          })),
        ),
      );
  });

/**
 * Calculate a score from a latitude and longitude.
 *
 * @param {Object} Object           Object used as an interface to get some properties.
 * @param {Object} Object.location  A location (city) containing lat and long.
 * @param {Number} Object.latitude  The city latitude to match the filter.
 * @param {Number} Object.longitude The city longitude to match the filter.
 * @return {Number}                 A score used to represent the location.
 */
const calculateLocationScore = ({ location, latitude, longitude }) => {
  const lat = Math.abs(location.lat - latitude);
  const long = Math.abs(location.long - longitude);
  const baseScore = 10 - (lat + long) / 2;
  const score = baseScore > 0 ? Math.round(baseScore) / 10 : 0;
  return score;
};

/**
 * Filter a list of cities.
 *
 * @param {Object} Object               Object used as an interface to get some properties.
 * @param {Array<Object>} Object.cities The list of cities to be filtered.
 * @param {String} Object.name          The city name to match the filter.
 * @param {Number} Object.latitude      The city latitude to match the filter.
 * @param {Number} Object.longitude     The city longitude to match the filter.
 * @return {Array<Object>}              A list of filtered cities.
 */
const filterCities = ({ cities, name, latitude, longitude }) =>
  cities
    .filter(
      (city) =>
        city.name.includes(name) &&
        (city.country === 'CA' || city.country === 'US') && // Restrict the filter to cities in the US and Canada.
        city.population > 5000, // Restrict the filter to population above 5000 people.
    )
    .map((location) => ({
      // Map the list in order to retrieve only the necessary properties.
      name: location.name,
      latitude: location.lat,
      longitude: location.long,
      score: calculateLocationScore({ location, latitude, longitude }),
    }));

/**
 * Export only the public methods.
 */
module.exports = {
  filterCities,
  readAndParseCSVFile,
};
