var Joi = require('joi');
var Boom = require('boom');

var qsSchema = Joi.object().keys({
    q: Joi.string().required(),
    longitude: Joi.number().min(-90).max(90),
    latitude: Joi.number().min(-90).max(90),
}).with('longitude', 'latitude').with('latitude', 'longitude');

function SuggestionsValidator() {

	this.validateQueryString = function(req, res, next) {
		Joi.validate(req.query, qsSchema, function (err, value) {
			if(err)	{
				return next(Boom.create(400, err)); 
			}
			//Bring back the sanitized / casted values into the query string
			req.query = value;
			return next();
		});
	}

	return this;
}

module.exports = SuggestionsValidator;

