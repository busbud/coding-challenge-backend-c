var cities = parseDataFile();

var CityModel = function(source_city) {
  return {
    name: source_city.name,
    alt_names: source_city.alt_name.split(','),
    country: getCountryName(source_city),
    state: getStateSymbol(source_city),
    latitude: source_city.lat,
    longitude: source_city.long
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
    let regex = new RegExp(query.search, 'i');

    if (regex.test(city.name) || regex.test(city.alt_name)) {
      results.push(CityModel(city));
    }
  });

  return results;
}

function getStateSymbol(city) {
  if (city.country === "US") return city.admin1;
  else {
    let map = {
      '01': "AB",
      '02': "BC",
      '03': "MB",
      '04': "NB",
      '05': "NL",
      '07': "NS",
      '08': "ON",
      '09': "PE",
      '10': "QC",
      '11': "SK",
      '12': "YT",
      '13': "NT"
    };

    return map[city.admin1];
  }
}

function getCountryName(city) {
  let map = {
    'CA': "Canada",
    'US': "USA"
  };

  return map[city.country];
}
