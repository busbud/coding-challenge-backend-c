const es = require('event-stream');
const sReduce = require('stream-reduce');
const path = require('path');
const fs = require('fs');
const rmDiac = require('./helpers/diacritics');


const parser = function (filename, callback) {

  if (!filename) {
    var err = new Error('Filename cannot be null, undefined or \'\'');
    return callback(err);
  }

  const file_path = path.relative(process.cwd(), filename);

  fs.createReadStream(file_path)
    .pipe(es.split('\n'))
    .pipe(es.mapSync(extractFields))
    .pipe(sReduce(reduceToArray, []))
    .on('data', (locations) => {
      // Remove header row and empty last element (EOL on every record in tsv)
      locations.shift();
      locations.pop();

      return callback(null, locations);
    });
};


const extractFields = function (row) {
  const fields = row.split('\t');

  return {
    id: fields[0],
    city: fields[1],
    canonical: rmDiac(fields[1]) ? rmDiac(fields[1]).toLowerCase() : null,
    state_province: fields[8] === 'CA' ? provinces[fields[10]] : fields[10],
    country: fields[8],
    lat: fields[4],
    lng: fields[5],
    population: parseInt(fields[14])
  };
};


const reduceToArray = function (acc, location) {
  acc.push(location);
  return acc;
};

const provinces = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '13': 'NT',
  '14': 'NU',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT'
};


module.exports = parser;