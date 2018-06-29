module.exports = {
  transform: results =>
    results.map(result => ({
      name: `${result.name}, ${result.state != "" ? result.state + ", " + result.country : result.country}`,
      longitude: result.location.longitude,
      latitude: result.location.latitude,
      score: result.score
    })),
  sort: results => results.sort((a, b) => (a.score > b.score ? -1 : a.score === b.score ? 0 : 1))
};
