const { transform, sort } = require("../response/transformer");

module.exports = (app, cityRepository) => {
  app.use((req, res, next) => {
    let { q, longitude, latitude, radius } = req.query;
    if (q === undefined) {
      return res.status(400).json({ error: "Missing 'q' parameter" });
    }

    if ((longitude === undefined && latitude !== undefined) || (longitude !== undefined && latitude === undefined)) {
      return res.status(400).json({ error: "Missing 'longitude' or 'latitude' parameters" });
    }

    if (radius !== undefined && (isNaN(radius) || radius < 1 || radius > 1000)) {
      return res.status(400).json({ error: "Bad parameter 'radius'. Value must be a number between 1 and 1000 km." });
    }

    next();
  });

  app.get("/suggestions", (req, res) => {
    let { q, longitude, latitude, radius = 100 } = req.query;

    if (longitude && latitude) {
      return cityRepository
        .findByNameAndLocation(q, { longitude, latitude }, radius)
        .then(transform)
        .then(sort)
        .then(suggestions => {
          if (suggestions.length === 0) {
            return res.status(404).send({ suggestions });
          }
          return res.status(200).json({ suggestions });
        });
    }

    return cityRepository
      .findByName(q)
      .then(transform)
      .then(sort)
      .then(suggestions => {
        if (suggestions.length === 0) {
          return res.status(404).send({ suggestions });
        }
        return res.status(200).json({ suggestions });
      });
  });
};
