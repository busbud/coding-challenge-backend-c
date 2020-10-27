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
 * Export only the public methods.
 */
module.exports = {
  readAndParseCSVFile,
};
