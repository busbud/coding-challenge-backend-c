module.exports = {
  transform: results =>
    results.map(result => ({
      name: `${result.name}, ${result.state}, ${result.country}`,
      longitude: result.location.longitude,
      latitude: result.location.latitude,
      score: result.score
    }))
};
