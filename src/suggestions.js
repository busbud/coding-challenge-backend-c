function getSuggestions(query, cities) {
  const { q, qLongitude, qLatitude } = query;

  if (q == '') return [];

  const validCities = findValidCities(cities, q);

  const scoredCities = getCityScores(validCities, qLongitude, qLatitude);

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

function getCityScores(cities, qLongitude, qLatitude) {
  //TODO: implement

  return cities;
}

function cleanCitiesObjects(cities) {
  return cities.map((city) => {
    const uniqueCityName = createUniqueName(city);
    return ({
      name: uniqueCityName,
      latitude: city.lat,
      longitude: city.long,
      score: city.score ? city.score : 0.5
    });
  });
}

function createUniqueName(city) {
  const { admin1, country, name } = city;
  return country == "US" ? [name, admin1, country].join(", ") : [name, country].join(", ");
}
module.exports.getSuggestions = getSuggestions;
