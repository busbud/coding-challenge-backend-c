/* eslint-disable no-console */

/**
 * This script must be executed once before starting the server.
 *
 * It parses the `cities_canada-usa.tsv` file given as a data source for the coding challenge
 * and creates, using a series of streams, a mongo-like Collection file to be used by NeDB.
 * at runtime.
 *
 * Some operations during the `transformer` stream take care of adjusting or creating new data
 * to facilitate the use of the data at runtime.
 *
 * It can be re-executed infinitely as it will replace the cities.db file each time.
 */
const fs = require('fs');
const latinize = require('latinize');
const parse = require('csv-parse');
const path = require('path');
const transform = require('stream-transform');

const City = require('../src/models/city').default;
const DatabaseWriter = require('../src/database/writer').default;


console.log('Beginning script');

// Source input
const tsvPath = path.resolve('./data/original/cities_canada-usa.tsv');
const input = fs.createReadStream(tsvPath.toString());

// Parser for tsv to receive piped data from fs.createReadStream
//    ('relax' and 'quote' parameters necessary to avoid parsing errors)
const parser = parse({
  auto_parse: true, columns: true, delimiter: '\t', relax: true, quote: '`',
});

// Debugging
parser.on('error', (err) => {
  console.log(`Error: ${err.message}`);
});

parser.on('finish', () => {
  console.log('Finished parsing');
});

// Adjust a few fields for a more desirable/consistent storage format in the DB
const transformer = transform((record, callback) => {
  const clone = Object.assign({}, record);

  if (record.alt_name !== '') {
    clone.alt_name = record.alt_name.split(',');
  }
  if (record.cc2 !== '') {
    clone.cc2 = record.cc2.split(',');
  }
  // Some values were numeric, others String. Cast all to String.
  clone.admin1 = String(record.admin1);
  clone.admin2 = String(record.admin2);
  clone.admin3 = String(record.admin3);
  clone.admin4 = String(record.admin4);
  clone.elevation = String(record.elevation);

  clone.searchableName = latinize(record.ascii).toUpperCase();

  callback(null, clone);
});

// Maps column names from the .tsv file to model field names for src/models/City so that
// DatabaseWriter can remain generic and reusable.
const fieldNamesMap = {
  id: 'geoNameId',
  name: 'name',
  ascii: 'asciiName',
  alt_name: 'alternateNames',
  lat: 'latitude',
  long: 'longitude',
  feat_class: 'featureClass',
  feat_code: 'featureCode',
  country: 'country',
  cc2: 'alternateCountryCodes',
  admin1: 'admin1',
  admin2: 'admin2',
  admin3: 'admin3',
  admin4: 'admin4',
  population: 'population',
  elevation: 'elevation',
  dem: 'dem',
  tz: 'tz',
  modified_at: 'modifiedAt',
  searchableName: 'searchableName',
};

// Write one object at a time into the database
const writer = new DatabaseWriter({ highWaterMark: 1, objectMode: true }, fieldNamesMap, City);

// Make sure to always re-create the Cities collection file every time this script is run.
fs.unlink(City.filePath(), (err) => {
  if (err) {
    console.log(err);
  }
  input.pipe(parser).pipe(transformer).pipe(writer);
});

