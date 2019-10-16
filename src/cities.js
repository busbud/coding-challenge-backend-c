const readCityData = require('../data/dataReader');

module.exports.getCities = async function () {
    try {
        let cities  = await readCityData.getCityData();
        let filteredCities = filterCities(cities);
        return filteredCities;

    } catch (err) {
        console.log('error');
    }
}

function filterCities (cities) {
    let filteredCities = cities.filter((city) => {
        if ((city.country === 'CA' || city.country === 'US') && city.population > 5000) {            
            return city;
        }
    });
    return filteredCities;
}