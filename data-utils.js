const COUNTRY_CODES = require('./data/countrycodes');
const ADMIN1_CODES = require('./data/admin1codes');

const filterDataByMinPopulation = (cities_data, min_population = 5000) =>
  Promise.resolve(cities_data.filter(c => c.population >= min_population))
;

const filterDataByCountry = (cities_data, array_of_accepted_countries = ['CA', 'US']) =>
  Promise.resolve(cities_data.filter(c => array_of_accepted_countries.includes(c.country)))
;

const sortDataByPopulationDesc = cities_data =>
  Promise.resolve(cities_data.sort((a, b) => b.population - a.population))
;

const dropUnusedDataFields = cities_data => {
  const keys_to_keep = ['id', 'name', 'ascii', 'alt_name', 'lat', 'long', 'country', 'admin1', 'population'];
  cities_data.forEach(c => Object.keys(c).forEach((k) => keys_to_keep.includes(k) || delete c[k]));
  return Promise.resolve(cities_data);
};

const replaceRegionCodesWithNames = cities_data => {
  cities_data.forEach(cities_data => {
    cities_data.admin1 = ADMIN1_CODES[cities_data.admin1];
    cities_data.country = COUNTRY_CODES[cities_data.country];
  });
  return Promise.resolve(cities_data);
};

const renameLatLong = cities_data => {
  cities_data.forEach(cities_data => {
    cities_data.latitude = cities_data.lat;
    delete cities_data.lat;
    cities_data.longitude = cities_data.long;
    delete cities_data.long;
  });
  return Promise.resolve(cities_data);
};

const addEasyDisplayName = cities_data => {
  cities_data.forEach(city_data => {
    const display_name_components = [];

    display_name_components.push(city_data.name);

    if (city_data.admin1 != null) {
      display_name_components.push(city_data.admin1);
    }

    if (city_data.country != null) {
      display_name_components.push(city_data.country);
    }

    city_data.easyDisplayName = display_name_components.join(', ');
  });
  return Promise.resolve(cities_data);
};

module.exports.filterDataByMinPopulation = filterDataByMinPopulation;
module.exports.filterDataByCountry = filterDataByCountry;
module.exports.sortDataByPopulationDesc = sortDataByPopulationDesc;
module.exports.dropUnusedDataFields = dropUnusedDataFields;
module.exports.replaceRegionCodesWithNames = replaceRegionCodesWithNames;
module.exports.renameLatLong = renameLatLong;
module.exports.addEasyDisplayName = addEasyDisplayName;
