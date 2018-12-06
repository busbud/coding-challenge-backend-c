const d3 = require('d3');
const fs = require('fs');

let cache;
const file = 'data/cities_canada-usa.tsv';

/**
 * Get all the cities from flat file or cache
 */
module.exports.import = () => new Promise(((resolve, reject) => {
  if (cache !== undefined && cache.length > 0) {
    resolve(cache);
  }

  fs.readFile(file, 'utf8', (error, data) => {
    if (error) {
      reject(error);
    }

    let finalResults = data;

    finalResults = data.replace(/"/g, '');
    finalResults = d3.tsvParse(finalResults);
    cache = finalResults;
    resolve(finalResults);
  });
}));
