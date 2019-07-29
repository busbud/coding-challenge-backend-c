// we only care about an endpoint so I'll add all the route handling to this
// module
const { Router } = require('express');

/**
 * ROUTE MODULE
 * @param {Module} controllers ./controllers/index.js module
 * @param {winston.Logger} logger instantiated logger
 */
module.exports = function (controllers, logger) {
  const router = Router();

  // /suggestions route handler
  router.get('/suggestions', async (req, res) => {
    if (!req.query.q) {
      return res
        .status(400)
        .send({msg: 'bad request'});
    }

    // extract query params
    const city = req.query.q;
    const coords = req.query.latitude && req.query.longitude ?
      {latitude: req.query.latitude, longitude: req.query.longitude} : null;

    let suggestions;

    // get suggestions
    try {
      suggestions = await controllers.suggestions.get(city, coords);
    }
    catch (err) {
      logger.error(err);
      return res
        .status(409)
        .send({msg: 'could not retrieve suggestions'});
    }

    let responseCode = suggestions.length ? 200 : 404;

    res
      .status(responseCode)
      .json(suggestions);
  });


  // 404
  router.use((req, res, next) => {
    res
      .status(404)
      .json({msg: 'not found'})
  });

  // error
  router.use((err, req, res, next) => {
    logger.error(err.stack);
    res
      .status(500)
      .json({msg: 'internal server error'});
  });

  return router;
};