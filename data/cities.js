'use strict';

import _          from 'lodash';
import parse      from 'csv-parse';
import {mapSync}  from 'event-stream';
import JSONStream from 'JSONStream';
import states     from './states.json';

const headers = [
  'id',
  'name',
  'ascii_name',
  'alternate_names',
  'latitude',
  'longitude',
  'feature_class',
  'feature_code',
  'country_code',
  'cc2',
  'admin1_code',
  'admin2_code',
  'admin3_code',
  'admin4_code',
  'population',
  'elevation',
  'dem',
  'timezone',
  'modification_date'
];

const numbers = ['latitude', 'longitude', 'population'];

process.stdin
  .pipe(parse({delimiter: '\t', quote: '', escape: ''}))
  .pipe(mapSync(_.compose(
    _.partialRight(_.pick, [
      'name',
      'ascii_name',
      'latitude',
      'longitude',
      'country_code',
      'state',
      'population'
    ]),

    // Retrieve state name
    c => _.assign(c, {state: states[`${c.country_code}.${c.admin1_code}`]}),

    // Convert numeric values
    c => _.assign(c, _(c).pick(numbers).mapValues(Number).value()),

      // Make an object with headers and array record
    _.partial(_.zipObject, headers))
  ))
  .pipe(JSONStream.stringify())
  .pipe(process.stdout);
