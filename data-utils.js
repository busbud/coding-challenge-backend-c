const countryCodes = require('./data/countrycodes');
const admin1Codes = require('./data/admin1codes');

const filterDataByPopulation = citiesData => citiesData.filter(cityData => cityData.population >= 5000);
const filterDataByCountry = citiesData => citiesData.filter(cityData => ['CA', 'US'].includes(cityData.country));
const filterByPopAndByCountry = citiesData => filterDataByPopulation(filterDataByCountry(citiesData));
const sortDataByPopulation = citiesData => citiesData.sort((cityDataA, cityDataB) => cityDataB.population - cityDataA.population);
const dropUnusedDataFields = citiesData => {
  const keysToKeep = ['id', 'name', 'lat', 'long', 'country', 'admin1', 'population'];
  citiesData.forEach(cityData => Object.keys(cityData).forEach((key) => keysToKeep.includes(key) || delete cityData[key]));
  return citiesData;
};
const makeRegionsReadable = citiesData => {
  citiesData.forEach(citiesData => {
    citiesData.admin1 = admin1Codes[citiesData.admin1];
    citiesData.country = countryCodes[citiesData.country];
  });
  return citiesData;
};

module.exports.filterByPopAndByCountry = filterByPopAndByCountry;
module.exports.sortDataByPopulation = sortDataByPopulation;
module.exports.dropUnusedDataFields = dropUnusedDataFields;
module.exports.makeRegionsReadable = makeRegionsReadable;