const Joi = require('joi');
const validator = require('express-joi-validation').createValidator({});

const querySchema = Joi.object({
  q: Joi.string().required(),
  latitude: Joi.number().min(-180).max(180),
  longitude: Joi.number().min(-180).max(180)
}).and('latitude', 'longitude');

module.exports = validator.query(querySchema);
