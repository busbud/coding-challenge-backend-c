/**
 * @function
 * @description Format the data retrieve in the search database into nice format
 * @param {Object} data - The raw data
 */
module.exports = (data) => {
  const suggestions = data.hits.hits
  if (suggestions.length === 0) {
    return []
  }

  const maxScore = data.hits.max_score

  return suggestions.map((suggestion) => ({
    name: suggestion._source.displayName,
    score: (suggestion._score / maxScore).toFixed(2),
    latitude: suggestion._source.location.lat,
    longitude: suggestion._source.location.lon
  }))
}
