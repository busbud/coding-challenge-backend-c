var express = require("express");
var suggestionController = require("../controllers/suggestion_controller");
var cache = require("../utils/cache");

var router = express.Router();

function cacheKey(req) {
    const {q,latitude,longitude} = req.query;
    return q + ":" + latitude + ":" + longitude;
}

router.get("/", cache(10 * 60, cacheKey), suggestionController.root);

module.exports = router;
