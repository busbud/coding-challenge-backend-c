const CITY_NAME_WEIGHT = 0.7; // it's an arbitary choice to give more weight to the city name than the position.
const LON_LAT_WEIGHT = 0.3;
const MAX_DISTANCE = 1000; // this could be reduced or increased
const MIN_POPULATION = 5000; // Even if all the cities present in the DB have more than 5000, I added a small protection concerning the minimum population number 

module.exports = {
  createDBQuery (cityName, longitude, latitude) {
    if(longitude && latitude) {
      return `select long_name as name, similarity(name, '${cityName}') as score, geolocation <@> point(${longitude},${latitude}) as distance, geolocation[0] as longitude, geolocation[1] as latitude from cities where name % '${cityName}' and population > ${MIN_POPULATION} order by 2 desc`;
    }
    return `select  long_name as name, similarity(name, '${cityName}') as score, geolocation[0] as longitude, geolocation[1] as latitude from cities where name % '${cityName}' and population > ${MIN_POPULATION} order by 2 desc`
  },
  
  improveSuggestionsScore (cities) {
    if(cities && cities.length > 0) {
      cities.map((suggestion) => {
        suggestion.score = Math.max(0, suggestion.score * CITY_NAME_WEIGHT  + ((1 - suggestion.distance / MAX_DISTANCE) * LON_LAT_WEIGHT));
        delete suggestion.distance; // remove distance property as it's not specified in the reponse output      
      })
      cities.sort((city1, city2) => this.hightestScore(city1, city2));
    }
  },

  hightestScore (city1, city2) {
    if(city1.score >= city2.score){
      return -1;
    } else {
      return 1;
    }
  },
}
