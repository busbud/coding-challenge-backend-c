const config = require('../config/config')
class CityModel {

    constructor(id, name, cityASCII, alternativeName, latitude, longitude, country, state) {
        this.id = id;
        this.name = name;
        this.cityASCII = cityASCII;
        this.alternativeNames = alternativeName;
        this.latitude = latitude;
        this.longitude = longitude;
        this.country = country === 'US' ? 'USA' : 'Canada'
        this.state = country === 'US' ? state : config.dataStore.canadianProvinceNameMap[state]
    }
}

module.exports = CityModel;