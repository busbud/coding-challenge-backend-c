const fromModel = (suggestion) => ({
  name: `${suggestion.name}, ${suggestion.admin1}, ${suggestion.country}`,
  latitude: suggestion.latitude,
  longitude: suggestion.longitude,
  score: suggestion.score,
});

export default {
  fromModel,
};
