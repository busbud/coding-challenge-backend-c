/**
 * Loads, parses, and filters CVS data file
 *
 * I do it sync here, for easier testing, simplicity  and because the data file is not such big (1.2MB)
 * Depending on the production conditions it may be reasonable to switch to stream API for parsing
 */

'use strict';

const { readFileSync } = require('fs');
const path = require('path');

const parse = require('csv-parse/lib/sync');

const MIN_POPULATION_SIZE = 5000;

/**
 * Reading and converting admin codes display name.
 * Will result in Map like 'US.CA' -> 'California'
 * Keeping only US and CA
 */
const adminCodes = new Map(
  parse(
    readFileSync(
      path.resolve(__dirname, '../data/admin1CodesASCII.txt'),
      'utf8'
    ),
    { skip_empty_lines: true, trim: true, delimiter: '\t', quote: null }
  )
    .filter(([code]) => /^CA|US/.test(code))
    .map(([code, name]) => [code, name])
);

/**
 * @type {{id: string, name: string, alt_name: string[], lat: number, long: number, population: number, feat_code: string}[]}
 */
const parsed = parse(
  readFileSync(
    path.resolve(__dirname, '../data/cities_canada-usa.tsv'),
    'utf8'
  ),
  {
    columns(header) {
      // we will keep only used columns
      return header.map(c => {
        // we will rename lat and long
        if (c === 'lat') return 'latitude';
        if (c === 'long') return 'longitude';
        // keep some others
        if (['name', 'alt_name', 'population', 'country', 'admin1'].includes(c))
          return c;
        // and ignore the rest
        return false;
      });
    },
    skip_empty_lines: true,
    trim: true,
    delimiter: '\t',
    quote: null,
    cast(value, { column }) {
      switch (column) {
        case 'latitude':
        case 'longitude':
          return parseFloat(value);

        case 'admin1':
          // Canadian codes are numeric and US are letter
          // little hacky, but for sake of simplicity and limits to two countries - will work
          // otherwise we will need to repass whole array just to add states
          return adminCodes.get(`CA.${value}`) || adminCodes.get(`US.${value}`);

        case 'population':
          return parseInt(value, 10);

        case 'alt_name':
          // convert alt_name into array of strings
          return value.split(',').filter(v => v);

        default:
          return value;
      }
    }
  }
);

// filtering by population size and keeping cities only
const FILTERED_CITIES = parsed.filter(
  ({ population, feat_code }) => population >= MIN_POPULATION_SIZE
);
module.exports.CITIES = FILTERED_CITIES;

// We will store longest and shortest lengths of city name for future validation/rating
const { shortest, longest } = FILTERED_CITIES.reduce(
  (res, { name, alt_name }) => {
    [name, ...alt_name].forEach(n => {
      if (n.length < res.shortest) res.shortest = n.length;
      else if (n.length > res.longest) res.longest = n.length;
    });
    return res;
  },
  { shortest: Infinity, longest: 0 }
);
module.exports.SHORTEST_CITY_NAME = shortest;
module.exports.LONGEST_CITY_NAME = longest;
