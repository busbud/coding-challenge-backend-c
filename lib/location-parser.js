/**
 *  converts cities into json
 */

var fs = require('fs');
var path = require('path');
var util = require('util');

var parse = require('csv-parse');
var _ = require('lodash');
var transform = require('stream-transform');

var Location = require('./location');


// fips mapping to CA provinces and territories
var FIPS = {
  'CA': {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '13': 'NT',
    '07': 'NS',
    '14': 'NU',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'YT',
    '05': 'NL'
  }
};

// country mapping
var COUNTRIES = {
  'CA': 'Canada',
  'US': 'USA'
}

function getState(row) {
  // US admin1 codes are already ISO codes
  if (row.country == 'US') {
    return row.admin1;
  }
  if (FIPS[row.country]) {
    return _.result(FIPS[row.country], row.admin1, row.admin1);
  }
  // default to undefined
}

function getCountry(countryCode) {
  return _.result(COUNTRIES, countryCode, countryCode);
}

function convert(row, cb) {
  cb(null, new Location({
    id: row.id,
    name: row.ascii,
    country: getCountry(row.country),
    state: getState(row),
    latitude: row.lat,
    longitude: row.long
  }));
}



module.exports = {
  convert: convert,
  load: function (dataFile, cb) {
    var input = fs.createReadStream(dataFile, {encoding: 'utf8'});
    var parser = parse({
      columns: true,
      delimiter: '\t',
      quote: false
    });
    
    input
      .on('error', cb)
      .pipe(parser)
      .pipe(transform(convert, cb));
    
  
  }
};