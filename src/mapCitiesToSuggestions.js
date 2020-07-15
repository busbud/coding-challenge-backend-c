exports.mapToSuggestions = async function(cities) {
  let body;
  if (cities.hits.total.value === 0) {
    body = {
      suggestions: [],
    };
  }

  const results = cities.hits.hits.map((hit) => {
    const source = hit._source;
    return {
      name: source.Name + ', ' + source.StateName + ', ' + source.CountryName,
      latitude: source.location.lat,
      longitude: source.location.lon,
      score: hit._score,
    };
  });
  body = {
    suggestions: results,
  };
  return body;
};
