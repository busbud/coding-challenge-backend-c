const { transform, sort } = require("../response/transformer");
const queryValidation = require("./queryValidation.js");

module.exports = (app, cityRepository) => {
  app.get("/suggestions", queryValidation, (req, res) => {
    let { q, longitude, latitude, radius = 100 } = req.query;
    let sendResponse = suggestions => {
      if (suggestions.length === 0) {
        return res.status(404).send({ suggestions });
      }
      return res.status(200).json({ suggestions });
    };

    if (longitude && latitude) {
      return cityRepository
        .findByNameAndLocation(q, { longitude, latitude }, radius)
        .then(transform)
        .then(sort)
        .then(sendResponse);
    }

    return cityRepository
      .findByName(q)
      .then(transform)
      .then(sort)
      .then(sendResponse);
  });
};
