/** Express router providing suggestions routes
 * @module routes/suggestion
 */
const { Router } = require("express");
const { asyncMiddleware, resultSuccess } = require("../utils");
const { searchQuery, searchQueryByLocation } = require("../services/suggestion");

const router = new Router();

/**
 * Route serves suggestion api.
 * @name suggestions?q
 * @function
 * @inner
 * @param {string} path - Express path
 * @param {callback} middleware - Async response middleware.
 */
router.get(
  "/",
  asyncMiddleware(async (req, res) => {
    const { q, longitude, latitude } = req.query;
    if (!q) {
      resultSuccess(res, []);
    }

    let result;
    if (!isNaN(latitude) && !isNaN(longitude)) {
      result = await searchQueryByLocation(q, longitude, latitude);
    } else {
      result = await searchQuery(q);
    }

    resultSuccess(res, result);
  })
);

module.exports = router;
