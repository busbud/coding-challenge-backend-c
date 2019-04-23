const fs = require('fs');
const d3 = require('d3-dsv');

let stringData = fs.readFileSync('./data/cities_canada-usa.tsv', 'utf8');
stringData = stringData.replace(/["]/g, ''); // Force data to conform to RFC 4180 before parsing it

const citiesData = d3.tsvParse(stringData, d3.autoType);
citiesData.forEach(cityData => {
  if (cityData.alt_name != null) {
    cityData.alt_name = d3.csvParseRows(cityData.alt_name);
    if (cityData.alt_name.length == 1) {
      cityData.alt_name = cityData.alt_name[0];
    }
  } else {
    cityData.alt_name = [];
  }
});
module.exports = citiesData;
