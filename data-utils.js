const countryCodes = require('./data/countrycodes');
const admin1Codes = require('./data/admin1codes');

const filterDataByPopulation = (citiesData, minPopulation = 5000) =>
  Promise.resolve(citiesData.filter(cityData => cityData.population >= minPopulation))
;

const filterDataByCountry = (citiesData, arrayOfAcceptedCountries = ['CA', 'US']) =>
  Promise.resolve(citiesData.filter(cityData => arrayOfAcceptedCountries.includes(cityData.country)))
;

const sortDataByPopulation = citiesData =>
  Promise.resolve(citiesData.sort((cityDataA, cityDataB) => cityDataB.population - cityDataA.population))
;

const dropUnusedDataFields = citiesData => {
  const keysToKeep = ['id', 'name', 'ascii', 'alt_name', 'lat', 'long', 'country', 'admin1', 'population'];
  citiesData.forEach(cityData => Object.keys(cityData).forEach((key) => keysToKeep.includes(key) || delete cityData[key]));
  return Promise.resolve(citiesData);
};

const makeRegionsReadable = citiesData => {
  citiesData.forEach(citiesData => {
    citiesData.admin1 = admin1Codes[citiesData.admin1];
    citiesData.country = countryCodes[citiesData.country];
  });
  return Promise.resolve(citiesData);
};

const renameLatLong = citiesData => {
  citiesData.forEach(citiesData => {
    citiesData.latitude = citiesData.lat;
    delete citiesData.lat;
    citiesData.longitude = citiesData.long;
    delete citiesData.long;
  });
  return Promise.resolve(citiesData);
};

const addEasyDisplayName = citiesData => {
  citiesData.forEach(cityData => {
    const displayNameComponents = [];

    displayNameComponents.push(cityData.name);

    if (cityData.admin1 != null) {
      displayNameComponents.push(cityData.admin1);
    }

    if (cityData.country != null) {
      displayNameComponents.push(cityData.country);
    }

    cityData.easyDisplayName = displayNameComponents.join(', ');
  });
  return Promise.resolve(citiesData);
};

module.exports.filterDataByPopulation = filterDataByPopulation;
module.exports.filterDataByCountry = filterDataByCountry;
module.exports.sortDataByPopulation = sortDataByPopulation;
module.exports.dropUnusedDataFields = dropUnusedDataFields;
module.exports.makeRegionsReadable = makeRegionsReadable;
module.exports.renameLatLong = renameLatLong;
module.exports.addEasyDisplayName = addEasyDisplayName;
