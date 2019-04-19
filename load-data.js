const fs = require('fs');
const LineStream = require('byline').LineStream;
const pump = require('pump');
const d3 = require('d3-dsv');

const filterDataByPopulation = citiesData => citiesData.filter(cityData => cityData.population >= 5000);
const filterDataByCountry = citiesData => citiesData.filter(cityData => ['CA', 'US'].includes(cityData.country));
const filterByPopAndByCountry = citiesData => filterDataByPopulation(filterDataByCountry(citiesData));
const sortDataByPopulation = citiesData => citiesData.sort((cityDataA, cityDataB) => cityDataB.population - cityDataA.population);
const dropUnusedDataFields = citiesData => {
  const keysToKeep = ['id', 'name', 'lat', 'long', 'country', 'admin1', 'population'];
  citiesData.forEach(cityData => Object.keys(cityData).forEach((key) => keysToKeep.includes(key) || delete cityData[key]));
  return citiesData;
};
const unwindArrayValues = citiesData => {
  let headerArray = citiesData.shift();
  return citiesData.map(cityValuesArray => {
    let cityData = {};
    cityValuesArray.forEach((value, columnIndex) => cityData[headerArray[columnIndex]] = value);
    return cityData;
  });
};

module.exports = path => new Promise( (resolve, reject) => {
  let citiesData = [];
  const citiesDataStream = fs.createReadStream('./data/cities_canada-usa.tsv', 'utf8');
  const lineStream = new LineStream();

  lineStream.on('data', line => {
    let rows = d3.tsvParseRows(line, d3.autoType);
    citiesData.push(...rows);
  });
  pump(citiesDataStream, lineStream, err => {
    if(!err) {
      resolve(sortDataByPopulation(filterByPopAndByCountry(dropUnusedDataFields(unwindArrayValues(citiesData)))));
    } else {
      reject(err)
    }
  });
});

module.exports.filterData = filterByPopAndByCountry;
module.exports.unwindArrayValues = unwindArrayValues;
module.exports.sortDataByPopulation = sortDataByPopulation;
module.exports.dropUnusedDataFields = dropUnusedDataFields;