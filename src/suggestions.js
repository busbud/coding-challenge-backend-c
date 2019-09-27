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

Current Weight:
0.8 for proportion of query string given
0.2 for population
*/

function getCityScores(cities, query, qLongitude, qLatitude) {
  // TODO: implement
  const largestPopulation = Math.max.apply(Math, cities.map((city) => {return city.population}));
  console.log(largestPopulation);
  const qLength = query.length;

  const basicScoreCities = cities.map((city) => {
    city.score = (query.length/city.name.length) * 0.8 + (city.population/largestPopulation)* 0.2
    return city;
  })

  return basicScoreCities;
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
  return country == "US" ? [name, admin1, country].join(", ") : [name, country].join(", ");
}

module.exports.getSuggestions = getSuggestions;
