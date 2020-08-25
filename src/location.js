const fs = require('fs');
const parse = require('csv-parse/lib/sync');

// constant names for canadian provinces as of https://en.wikipedia.org/wiki/List_of_FIPS_region_codes_(A%E2%80%93C)#CA:_Canada
const provincesCA = [
  'Alberta','British Columbia', 'Manitoba', 'New Brunswick', 'Newfoundland and Labrador',
  'Nova Scotia', 'Ontario', 'Prince Edward Island', 'Quebec', 'Saskatchewan', 'Yukon',
  'Northwest Territories', 'Nunavut'
];

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