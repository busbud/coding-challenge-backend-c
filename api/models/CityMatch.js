'use strict';

/**
 * Class used to store searched results
 */
class CityMatch  {

    /**
     *
     * @param city City object
     * @param score between 0 and 1, higher is better
     * @param distance expressed in kilometers
     */
    constructor(city, score, distance) {
        this.name= "".concat(city.name, " (",city.country, "), ",city.tz);
        this.longitude=city.longitude;
        this.latitude=city.latitude;
        this.score=score;
        this.distance=distance;
    }
}

module.exports = CityMatch;