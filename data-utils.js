const filterDataByPopulation = citiesData => citiesData.filter(cityData => cityData.population >= 5000);
const filterDataByCountry = citiesData => citiesData.filter(cityData => ['CA', 'US'].includes(cityData.country));
const filterByPopAndByCountry = citiesData => filterDataByPopulation(filterDataByCountry(citiesData));
const sortDataByPopulation = citiesData => citiesData.sort((cityDataA, cityDataB) => cityDataB.population - cityDataA.population);
const dropUnusedDataFields = citiesData => {
  const keysToKeep = ['id', 'name', 'lat', 'long', 'country', 'admin1', 'population'];
  citiesData.forEach(cityData => Object.keys(cityData).forEach((key) => keysToKeep.includes(key) || delete cityData[key]));
  return citiesData;
};

module.exports.filterByPopAndByCountry = filterByPopAndByCountry;
module.exports.sortDataByPopulation = sortDataByPopulation;
module.exports.dropUnusedDataFields = dropUnusedDataFields;