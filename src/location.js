const fs = require('fs');
const parse = require('csv-parse/lib/sync');
const levenshtein = require('js-levenshtein')

// constant names for canadian provinces as of https://en.wikipedia.org/wiki/List_of_FIPS_region_codes_(A%E2%80%93C)#CA:_Canada
const PROVINCES_CA = [
  '', 'Alberta','British Columbia', 'Manitoba', 'New Brunswick', 'Newfoundland and Labrador',
  '', 'Nova Scotia', 'Ontario', 'Prince Edward Island', 'Quebec', 'Saskatchewan', 'Yukon',
  'Northwest Territories', 'Nunavut'
];
const MAX_LEVEN = 2; // maximum allowed levenshtein distance
const MAX_DIST = 1500; // maximum allowed spatial distance in km

const extractDataFromCSV = () => {
  // read data from csv file
  let data = fs.readFileSync('./data/cities_canada-usa.tsv', 'utf-8');
  // parse csv
  data = parse(data,{
    delimiter: '\t',
    skip_lines_with_error: true
  });
  // extract the useful information: ascii and alt names, lat, long
  data = data.map((item) => ({
    ascii: item[1],
    alt: item[3],
    country: item[8],
    state: item[10],
    lat: parseFloat(item[4]),
    long: parseFloat(item[5])
  }));
  return data;
};

// calculate distance between two coordinates, as taken from and explained by
// https://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
const spatialDistance = (lat1, lon1, lat2, lon2) => {
  const p = 0.017453292519943295; // Math.PI / 180
  const R2 = 12742; // 2 * R; R = 6371 km
  const a = 0.5 - Math.cos((lat2 - lat1) * p)/2 + Math.cos(lat1 * p) * Math.cos(lat2 * p) * (1 - Math.cos((lon2 - lon1) * p))/2;
  return R2 * Math.asin(Math.sqrt(a))
};

// calculate confidence score based on levenshtein distance
const score_leven = (leven, maxDeviation) => 1.0 - Math.min(1.0, 1.0*leven/maxDeviation);

// calculate confidence score based on location distance
const score_location = (lat_query, long_query, lat_candidate, long_candidate, maxDeviation) => {
  const dist = spatialDistance(lat_query, long_query, lat_candidate, long_candidate);
  return 1.0 - Math.min(1.0, 1.0*dist/maxDeviation);
};

const search = (query, lat, long) => {
  // extract data
  const data = extractDataFromCSV();

  // 1. search cities by ascii name
  // calculate levenshtein distance and store it
  data.forEach((item) => {
    item.leven = levenshtein(query, item.ascii);
  });
  // candidates have a levenshtein distance of <= 10 to query
  let candidates = data.filter((city) => city.leven < MAX_LEVEN);

  // 2. if no matches: search cities by alternative names
  if (candidates.length === 0){
    const smallestLevenshtein = (arr) => {
      let dist = arr.map((el) => levenshtein(query, el));
      return Math.min(...dist);
    };
    data.forEach((item) => {
      item.leven = smallestLevenshtein(item.alt.split(','))
    });
    candidates = data.filter((city) => city.leven < MAX_LEVEN);
  }
  // 3. if still empty return, else calculate confidence based on levenshtein distance
  if (candidates.length === 0){
    return [];
  } else {
    candidates = candidates.map((item) => {
      let name = `${item.ascii}, ${item.country == 'US' ? item.state : PROVINCES_CA[parseInt(item.state,base=10)]}, ${item.country}`;
      let score = score_leven(item.leven, MAX_LEVEN);
      return {
        'name': name,
        'latitude': `${item.lat}`,
        'longitude': `${item.long}`,
        'score': score
      };
    });
  }
  // 4. optionally enhance confidence score with location information
  if (lat !== null || long !== null){
    candidates.forEach((el) => {
      el.score = el.score * score_location(lat, long, parseFloat(el.latitude), parseFloat(el.longitude), MAX_DIST);
    });
  }

  // return list of candidates sorted by confidence score
  return candidates.sort((a, b) => b.score - a.score);
};

module.exports.search = search;