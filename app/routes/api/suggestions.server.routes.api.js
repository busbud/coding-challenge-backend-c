'use strict';
/**
 * suggestions.server.routes.api.js
 * ------------------------------
 */
var _ = require('lodash');
var async = require('async');
var citiesController = require('../controllers/cities.server.routes.controller');

module.exports.get = function(req, res) {
	(req.query.latitude && req.query.longitude
	  ? citiesController.findNearStartsWith(req)
	  : citiesController.findStartsWith(req))
	.then(function(suggestions) {
		if (!suggestions || suggestions.length < 1) {
			return res.apiError("NotFound", new Error('No matching results'), null, 404);
		}
		return res.apiSuccess(suggestions);
	}).catch(function(e) {
		//if (process.env.NODE_ENV === 'development')
    		return res.apiError("Oops! Something went wrong!", e, null, 500);
	});
};