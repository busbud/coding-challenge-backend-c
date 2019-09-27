const geolib = require('geolib');

function getSuggestions(query, cities) {
  const { q, longitude, latitude } = query;

  if (q == '') return [];

  const validCities = findValidCities(cities, q);

  const scoredCities = getCityScores(validCities, q, longitude, latitude);

  const suggestedCities = cleanCitiesObjects(scoredCities);

  return suggestedCities;
}

function findValidCities(cities, q) {
  const validCities = cities.filter((city) => {
    if (city.name.startsWith(q)) return true;

    validAltNames = city.alt_name.filter((altName) => {
      return altName.startsWith(q)
    })

    if (validAltNames.length != 0) {
      city.name = validAltNames[0];
      return true;
    }
    return false;
  });

  return validCities;
}

/* Ideally, the city score should be based on a confidence of how frequently people search
for the city, but we currently have no data for that so we will base confidence on the proportion of
the name string that is provided in the query string, weighted with population (assumption that people
usually search for places with larger populations)

Basic Weight:
0.8 for proportion of query string given
0.2 for population
*/

function getCityScores(cities, query, qLongitude, qLatitude) {
  const largestPopulation = Math.max.apply(Math, cities.map((city) => {return city.population}));
  console.log(largestPopulation);
  const qLength = query.length;
  const hasLocation = qLongitude && qLatitude

  let citiesWithScores = cities.map((city) => {
    city.score = (query.length/city.name.length) * 0.8 + (city.population/largestPopulation)* 0.2;
    return city;
  })

  if (hasLocation) {
    console.log("calculating location");
    const citiesWithLocDifference = citiesWithScores.map((city) => {
      city.locDifference = geolib.getDistance(
        {latitude: city.lat, longitude: city.long},
        {latitude: qLatitude, longitude: qLongitude}
      );
      return city;
    });

    const farthestDistance = Math.max.apply(Math, citiesWithLocDifference.map((city) => {return city.locDifference}));

    /* weight original score and distance score 50/50
    New weight distribution:
    0.5 for location
    0.4 for proportion of query string given
    0.1 for population
     */
    citiesWithGeoScores = citiesWithLocDifference.map((city) => {
      city.score = city.score * 0.5 + (1-(city.locDifference/farthestDistance)) * 0.5;
      return city;
    })
  }
  return citiesWithScores;
}

function cleanCitiesObjects(cities) {
  const cleanedCities = cities.map((city) => {
    const uniqueCityName = createUniqueName(city);
    return ({
      name: uniqueCityName,
      latitude: city.lat,
      longitude: city.long,
      // TODO: use the actual score
      score: city.score ? city.score : Math.random()
    });
  });
  return cleanedCities.sort((city1, city2) => { return city2.score - city1.score })
}

function createUniqueName(city) {
  const { admin1, country, name } = city;
  return country == 'US' ? [name, admin1, country].join(', ') : [name, country].join(', ');
}

module.exports.getSuggestions = getSuggestions;
