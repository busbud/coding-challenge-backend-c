const fs = require('fs');
const parse = require('csv-parse/lib/sync');

// constant names for canadian provinces as of https://en.wikipedia.org/wiki/List_of_FIPS_region_codes_(A%E2%80%93C)#CA:_Canada
const PROVINCESCA = [
  'Alberta','British Columbia', 'Manitoba', 'New Brunswick', 'Newfoundland and Labrador',
  'Nova Scotia', 'Ontario', 'Prince Edward Island', 'Quebec', 'Saskatchewan', 'Yukon',
  'Northwest Territories', 'Nunavut'
];
// one degree of latitude in kilometres
const LAT2KM = 111.0

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
  const a = 0.5 - Math.cos((lat2 - lat1) * p)/2 + 
    Math.cos(lat1 * p) * Math.cos(lat2 * p) * 
    (1 - Math.cos((lon2 - lon1) * p))/2;

  return R2 * Math.asin(Math.sqrt(a))
}; 

const search = (query, lat, long) => {
  
  const data = extractDataFromCSV();
  console.log(data)




  return [
    {
      "query": query,
      "latitude": lat,
      "longitude": long
  }
  ];
};

search('a','b','c');

module.exports.search = search;