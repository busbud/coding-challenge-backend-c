'use strict';
/**
 * Module dependencies.
 */

var cacheManager = require('cache-manager');
var memoryCache = cacheManager.caching({store: 'memory', max: 100, ttl: 60});
var citiesController = require('../controllers/cities.server.routes.controller');

/**
* Handles a validated get request
*
* @param {Object} request object
* @param {Object} response object
*/

module.exports.get = function(req, res) {
	memoryCache.wrap(JSON.stringify(req.query), () => {
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