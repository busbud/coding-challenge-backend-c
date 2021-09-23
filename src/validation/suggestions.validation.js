const Joi = require('joi');
const validator = require('express-joi-validation').createValidator({});

const querySchema = Joi.object({
  name: Joi.string().required()
});

export const suggestionValidator = validator.query(querySchema);
