var express = require("express");

var cache = require("../utils/cache");
var rateLimiter = require('../utils/rate_limiter');
var setup = require("../properties");
var suggestionController = require("../controllers/suggestion_controller");

var router = express.Router();

function cacheKey(req) {
  const { q, latitude, longitude } = req.query;
  return q + ":" + latitude + ":" + longitude;
}

router.get(
  "/",
  rateLimiter,
  cache(setup.app.cache.suggestions.ttl, cacheKey),
  suggestionController.root
);

module.exports = router;
