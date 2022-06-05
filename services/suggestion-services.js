module.exports = {
  createDBQuery (cityName, longitude, latitude) {
    if(longitude && latitude) {
      return `select long_name as name, similarity(name, '${cityName}') as score, geolocation <@> point(${longitude},${latitude}) as distance, geolocation[0] as longitude, geolocation[1] as latitude from cities where name % '${cityName}' order by 2 desc`;
    }
    return `select  long_name as name, similarity(name, '${cityName}') as score, geolocation[0] as longitude, geolocation[1] as latitude from cities where name % '${cityName}' order by 2 desc`
  },
  
  improveSuggestionsScore (cities) {
    if(cities && cities.length > 0) {
      cities.map((suggestion) => {
        suggestion.score = Math.max(0, suggestion.score * 0.7 + ((1 - suggestion.distance / 1000) * 0.3));
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