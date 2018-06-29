const { transform, sort } = require("../response/transformer");
const queryValidation = require("./queryValidation.js");

module.exports = (app, cityRepository) => {
  app.use(queryValidation);

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
