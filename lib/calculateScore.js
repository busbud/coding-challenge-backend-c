const geodist = require('geodist');
require('string_score');
const levenshtein = require('fast-levenshtein');


const calculateScoreForAllAndSort = (cities, query, latitude, longitude) => {
  let intervals = {};
  cities.forEach((city) => {
    city.weights = {};
  });

  calculateScoreForPopulation(cities);
  calculateScoreForString(cities, query);
  if(!isNaN(latitude) && !isNaN(longitude)){
    calculateScoreForLocation(cities, latitude, longitude);
  }

  let results = [];
  let qtty = 0;
  cities.forEach((city) => {
    if(!isNaN(city.weights.location)){
      city.score =
        city.weights.location    * 0.5 +
        city.weights.population  * 0.3 +
        city.weights.levenshtein * 0.2;
    } else {
      city.score =
        city.weights.population  * 0.4 +
        city.weights.levenshtein * 0.6;
    }
    results.push({
        id: city.value.id,
        name: city.value.name,
        displayName: city.value.displayName,
        latitude: city.value.latitude,
        longitude: city.value.longitude,
        country: city.value.country,
        population: city.value.population,
        //distance: city.value.distanceToUser, //test&debug purpose
        //scores: city.weights, //test&debug purpose
        score: city.score
    });
  });

  //TODO: this magic number should be configurable
  return results.sort((obj1, obj2) => obj2.score - obj1.score).slice(0, 10);
};

const calculateScoreForPopulation = (cities) => {
  const maxPop = Math.max.apply(Math,cities.map(c => c.value.population))
  const minPop = Math.min.apply(Math,cities.map(c => c.value.population))
   cities.forEach((city) => {
    //normalizer is kind of lame. Adding 1 to the max to avoid 0/0 in case min=value=max
    city.weights.population = normalizeNumber(city.value.population, minPop, maxPop+1);
  });
}

const calculateScoreForLocation = (cities, latitude, longitude) => {
  cities.forEach((city) => {
    city.value.distanceToUser = geodist (
      { lat: latitude, lon: longitude },
      { lat: city.value.latitude, lon: city.value.longitude }
    );
  });
  const maxDistance = Math.max.apply(Math,cities.map(c => c.value.distanceToUser))
  const minDistance = Math.min.apply(Math,cities.map(c => c.value.distanceToUser))
  cities.forEach((city) => {
    //normalizer is kind of lame. Adding 1 to the max to avoid 0/0 in case min=value=max
    city.weights.location = 1 - normalizeNumber(city.value.distanceToUser, minDistance, maxDistance+0.00001);
  });
}

const calculateScoreForString = (cities, query) => {
  cities.forEach((city) => {
    city.value.levenshtein = levenshtein.get(city._key_, query);;
  });
  const maxLevensh = Math.max.apply(Math,cities.map(c => c.value.levenshtein))
  const minLevensh = Math.min.apply(Math,cities.map(c => c.value.levenshtein))
  cities.forEach((city) => {
    //normalizer is kind of lame. Adding 1 to the max to avoid 0/0 in case min=value=max
    city.weights.levenshtein = 1 - normalizeNumber(city.value.levenshtein, minLevensh, maxLevensh+0.00001);
  });
}

const normalizeNumber = (num, min, max) => {
  return (num - min) / (max - min);
}

exports.calculateScoreForAllAndSort = calculateScoreForAllAndSort;
