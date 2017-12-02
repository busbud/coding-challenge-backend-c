'use strict';

class CityMatch  {
   constructor(city, score, distance) {
        this.name= "".concat(city.name, ", ",city.country, ", ",city.tz);
        this.longitude=city.longitude;
        this.latitude=city.latitude;
        this.score=score;
        this.distance=distance;
    }
}

module.exports = CityMatch;