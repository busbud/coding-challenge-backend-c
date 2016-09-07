var functions = require('../functions');
var cities = parseDataFile();

var CityModel = function(source_city) {
  return {
    name: source_city.name,
    alt_names: source_city.alt_name.split(','),
    country: functions.getCountryName(source_city),
    state: functions.getStateSymbol(source_city),
    latitude: source_city.lat,
    longitude: source_city.long,
    score: 0
  };
};

module.exports = {
  getByQuery: getByQuery
};

function parseDataFile() {
  let parse = require('csv-parse/lib/sync'); // we're gonna use syncronous method... it's slow, but we need the data before proceding anyway
  let fs = require('fs');

  let citiesFile = fs.readFileSync(__dirname+'/../data/cities_canada-usa.tsv');

  return parse(
    citiesFile,
    {
      delimiter: "\t",
      quote: "",
      columns: true
    });
}

function getByQuery(query) {
  let results = [];

  cities.forEach(city => {
    let regex = new RegExp(query, 'i');

    if (regex.test(city.name) || regex.test(city.alt_name)) {
      results.push(CityModel(city));
    }
  });

  return results;
}
