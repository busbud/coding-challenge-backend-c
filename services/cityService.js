const readData = require("../data/dataReader");

async function getCities() {
  try {
    return createCities(await readData());
  } catch (ex) {
    console.log("ex", ex.message);
  }
}

function createCities(data) {
  const cities = [];

  data.map(item => {
    const city = createCity(item);
    if (city) cities.push(city);
  });
  return cities;
}

function createCity(item) {
  const countryCode = item[8];
  const population = Number(item[14]);

  let city;

  if (countryCode === "CA" || (countryCode === "US" && population > 5000)) {
    city = {
      name: item[1],
      alternatenames: item[3],
      latitude: item[4],
      longitude: item[5],
      adminCode: item[10],
      countryCode
    };
  }
  return city;
}

module.exports = getCities;
