const fs = require('fs');
const d3 = require('d3-dsv');

let string_data = fs.readFileSync('./data/cities_canada-usa.tsv', 'utf8');
string_data = string_data.replace(/["]/g, ''); // Force data to conform to RFC 4180 before parsing it

const cities_data = d3.tsvParse(string_data, d3.autoType);

// post processing, change comman-separated alt_name field into array of alt names
cities_data.forEach(city_data => {
  if (city_data.alt_name != null) {
    city_data.alt_name = d3.csvParseRows(city_data.alt_name); // unwrap comma-separated alternative names
    if (city_data.alt_name.length === 1) {
      city_data.alt_name = city_data.alt_name[0]; // unwrap nested array
    }
  } else {
    city_data.alt_name = []; // always provide a default so that we can alt_name.forEach
  }
});
module.exports = cities_data;
