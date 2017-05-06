var Joi = require('joi');

module.exports = {
	validateQueryParams: function(req, res, next) {
		var result = null;
		switch(req.path) {
		    case '/suggestions':
		   		result = Joi.validate(req.query, getSuggestionsQueryParamsSchema)
		        break;
		    default:
		        break
		}
		if (!result || !result.error) return next();
		return res.apiError('Validation error', result.error, null, 400);
	}
};

var getSuggestionsQueryParamsSchema = Joi.object().keys({
    q: Joi.string().required(),
    latitude: Joi.number().min(-90).max(90),
    longitude: Joi.number().min(-180).max(180)
}).with('latitude', 'longitude')
  .with('longitude', 'latitude')
  .unknown(false);