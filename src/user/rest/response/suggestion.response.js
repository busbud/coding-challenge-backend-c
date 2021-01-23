const of = (suggestion) => ({
  name: suggestion.fullSuggestion,
  latitude: suggestion.location.lat,
  longitude: suggestion.location.lon,
  score: suggestion.score.toFixed(1),
});

const fromList = (suggestions) => suggestions.map((suggestion) => of(suggestion));

const success = (res, suggestions) => {
  const statusCode = suggestions.length === 0 ? 404 : 200;
  res.writeHead(statusCode, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    suggestions: fromList(suggestions),
  }));
};

module.exports.success = success;
