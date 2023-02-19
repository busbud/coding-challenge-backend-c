const Ajv = require("ajv");
const router = require('express').Router();
const locationController = require('../controllers/location.controller');


// Request validation
const ajv = new Ajv();
const schema = {
  type: "object",
  properties: {
    q: { type: "string" },
    latitude: { type: "string" },
    longitude: { type: "string" }
  },
  required: ["q"],
  additionalProperties: false,
}

/**
 * Additional, non schema, business logic validation
 * @param {Object} data request obj
 */
function locationReqValidator(data) {
  const { q, latitude, longitude } = data;
  const isNumeric = (coord) => !isNaN(parseFloat(coord)) && isFinite(coord);
  const isAlpha = (str) => /^[a-zA-Z]+$/.test(str);

  if (latitude || longitude) {
    if (!isNumeric(latitude) || !isNumeric(longitude)) {
      return false;
    }
  }

  return isAlpha(q);
}

/**
 * Request validation middleware
 * @param {Object} req 
 * @param {Object} res 
 * @param {Function} next 
 */
function validateRequest(req, res, next) {
  const valid = ajv.validate(schema, req.query);
  if (!valid) {
    return res.status(400).json({ error: 'Invalid request' });
  } else {
    if (!locationReqValidator(req.query)) {
      return res.status(400).json({ error: 'Invalid request' });
    }
    next();
  }
}


router.get('/suggestions', [validateRequest], locationController.getSuggestions);

module.exports = router;