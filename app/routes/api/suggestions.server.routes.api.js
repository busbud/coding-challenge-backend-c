'use strict';
/**
 * suggestions.server.routes.api.js
 * ------------------------------
 */
var multiCache = require('../../utils/multicache');
var citiesController = require('../controllers/cities.server.routes.controller');

module.exports.get = function(req, res) {
	var cacheKey = JSON.stringify(req.query);
	multiCache.wrap(cacheKey => {
    	return req.query.latitude && req.query.longitude
		  ? citiesController.findNearStartsWith(req)
	  	  : citiesController.findStartsWith(req);
    }).then(suggestions => {
		if (!suggestions || suggestions.length < 1) {
			return res.apiError("NotFound", new Error('No matching results'), null, 404);
		}
		return res.apiSuccess(suggestions);
	}).catch(function(e) {
		return res.apiError("Oops! Something went wrong!", e, null, 500);
	});

};