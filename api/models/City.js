'use strict';

/**
 * We want to optimize the memory usage so we define City object with only necessary attributes
 */
class City {
   constructor(informations) {
        this.id=informations.id;
        this.name=informations.name;
        this.country=informations.country;
        this.longitude=informations.long;
        this.latitude=informations.lat;
        this.tz=informations.tz;
        this.population=informations.population;
    }
}

module.exports = City;