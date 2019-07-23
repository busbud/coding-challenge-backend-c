// General Libraries
const express = require('express');
// Application Code
const { getSuggestions } = require('../domain/suggestor');
const { getSuggestionParameters, serializeCity  } = require('./routes.helper');
var router = express.Router();
const HTTP_OK = 200;
const HTTP_BAD_REQUEST = 400;
const HTTP_NOT_FOUND = 404;


/**
 * Implements the basig [GET] /suggestions request
 */
router.get('/', async function(req, res) {
  // fetch and validate parameters
  const { q, coordinate, is_valid, error_msg } = getSuggestionParameters(req.query);
  if (!is_valid) {
    // return 400 if parameters aren't valid
    return res.status(HTTP_BAD_REQUEST).json({ error: error_msg });
  }
  // retrieve suggestions
  const suggestions = await getSuggestions(q, coordinate);
  // format suggestions
  res.status((suggestions.length > 0) ? HTTP_OK : HTTP_NOT_FOUND).json({
    suggestions: suggestions.map((city) => serializeCity(city))
  });
});

module.exports = router;
