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
 * Filter a list of cities.
 *
 * @param {Object} Object               Object used as an interface to get some properties.
 * @param {Array<Object>} Object.cities The list of cities to be filtered.
 * @return {Array<Object>}              A list of filtered cities.
 */
const filterCities = ({ cities, name }) =>
  cities
    .filter(
      (city) =>
        city.name.includes(name) &&
        (city.country === 'CA' || city.country === 'US') && // Restrict the filter to cities in the US and Canada.
        city.population > 5000, // Restrict the filter to population above 5000 people.
    )
    .map((city) => ({
      // Map the list in order to retrieve only the necessary properties.
      name: city.name,
      latitude: city.lat,
      longitude: city.long,
    }));

/**
 * Export only the public methods.
 */
module.exports = {
  filterCities,
  readAndParseCSVFile,
};
