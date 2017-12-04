'use strict';

/**
 * We want to optimize the memory usage so we define City object with only necessary attributes
 */
class City {
   constructor(informations) {
        this.id=informations.id;
        this.name=informations.name;
        this.normalizedName=informations.normalizedName;
        this.country=informations.country;
        this.longitude=informations.longitude;
        this.latitude=informations.latitude;
        this.tz=informations.tz;
        this.population=informations.population;
    }
}

module.exports = City;