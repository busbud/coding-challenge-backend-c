module.exports = (app, cityRepository) => {
  app.use((req, res, next) => {
    let { q, longitude, latitude } = req.query;
    if (q === undefined) {
      return res.status(400).json({ error: "Missing 'q' parameter" });
    }

    if ((longitude === undefined && latitude !== undefined) || (longitude !== undefined && latitude === undefined)) {
      return res.status(400).json({ error: "Missing 'longitude' or 'latitude' parameters" });
    }

    next();
  });

  app.get("/suggestions", (req, res) => {
    let { q, longitude, latitude } = req.query;

    if (longitude && latitude) {
      cityRepository
        .findByNameAndLocation(q, { longitude, latitude })
        .then(transform)
        .then(sort)
        .then(suggestions => {
          if (suggestions.length === 0) {
            return res.status(404).send({ suggestions });
          }
          return res.status(200).json({ suggestions });
        });
    } else {
      cityRepository
        .findByName(q)
        .then(transform)
        .then(sort)
        .then(suggestions => {
          if (suggestions.length === 0) {
            return res.status(404).send({ suggestions });
          }
          return res.status(200).json({ suggestions });
        });
    }
  });

  const transform = results =>
    results.map(result => ({
      name: `${result.name}, ${result.state != "" ? result.state + ", " + result.country : result.country}`,
      longitude: result.location.longitude,
      latitude: result.location.latitude,
      score: result.score
    }));

  const sort = results => results.sort((a, b) => (a.score > b.score ? -1 : a.score === b.score ? 0 : 1));
};
