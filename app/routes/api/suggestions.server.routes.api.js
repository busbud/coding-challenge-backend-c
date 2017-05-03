'use strict';
/**
 * suggestions.server.routes.api.js
 * ------------------------------
 */
var _ = require('lodash');
var citiesController = require('../controllers/cities.server.routes.controller');

module.exports.get = function(req, res) {
	var statusMessages = [];

	// Validate main search param
	if (!req.query.q) {
		return res.apiError('Bad request.', new Error('Request failed'), ['Missing query parameter(s)', 'Parameter \'q\' is required']);
	}

	// Validate optional lat/lng params
	var hasOptLatLng = false;
	if (req.query.latitude || req.query.longitude) {
		if (req.query.latitude && !req.query.longitude) {
			statusMessages.push('Query parameter latitude was provided with no corresponding longitude.');
		}

		if (!req.query.latitude && req.query.longitude) {
			statusMessages.push('Query parameter longitude was provided with no corresponding latitude.');
		}

		if (req.query.latitude && req.query.longitude) {
			if (validateLat(req.query.latitude) && validateLng(req.query.longitude)) {
				hasOptLatLng = true;
			} else {
				statusMessages.push('Invalid latitude and/or longitude parameter(s).');
			}
		}

		if (!hasOptLatLng) statusMessages.push('Coordinates were ignored.');
	}

	citiesController.find(req, function(err, results){
		// le status: true devrait pas work.. pas le choice apiError
		return res.apiSuccess(results, statusMessages);
	});
};

function validateLat(lat) {
	return Number.isFinite(lat) && Math.abs(lat) <= 90;
}

function validateLng(lng) {
	return Number.isFinite(lng) && Math.abs(lng) <= 180;
}