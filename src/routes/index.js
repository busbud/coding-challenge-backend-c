module.exports = (app, cityRepository) => {
  app.use((req, res, next) => {
    let query = req.query.q;
    let { longitude, latitude } = req.query;

    if (query === undefined) {
      return res.status(400).send();
    }

    if ((longitude === undefined && latitude !== undefined) || (longitude !== undefined && latitude === undefined)) {
      return res.status(400).send();
    }

    next();
  });

  app.get("/suggestions", (req, res) => {
    let query = req.query.q;
    let { longitude, latitude } = req.query;

    if (longitude && latitude) {
      cityRepository
        .findByNameAndLocation(query, { longitude, latitude })
        .then(transform)
        .then(suggestions => {
          if (suggestions.length === 0) {
            return res.status(404).send({ suggestions });
          }
          return res.status(200).json({ suggestions });
        });
    } else {
      cityRepository
        .findByName(query)
        .then(transform)
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
};
