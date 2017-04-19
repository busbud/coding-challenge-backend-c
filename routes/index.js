'use strict'

/**
 * Module Dependencies
 */
const 
	path  = require('path'),
	utils = require('../lib/utils');

/**
 * Model Schema
 */
const City = require(path.join(__basedir, MODELS_DIR)).City;


/**
* Endpoints
*/
server.get('/suggestions', (req, res, next) => {
	
	// Set the default response body: an empty list of suggestions.
	var responseBody = {'suggestions': [] };

	// Stop process if 'q' query param was not provided.
	// Set response code as 404 - NotFound.
	if (typeof req.query.q == 'undefined') {
		res.send(404, responseBody);
		return next();
	}

	// Set defaults for optional query params.
	// Note: Advanced validation of query params could be done here.

	// limit: maximum number of results to add in the response.
	// Not used in the implementation.
	//req.query.limit = Number(req.query.limit) || 10;
	// latitude and longitude are converted into Number, or set as null if not provided.
	req.query.latitude = Number(req.query.latitude) || null;
	req.query.longitude = Number(req.query.longitude) || null;

	// Search in database for documents with 'name' matching to the provided query string.
	// Select only relevant fields:
	//   - name, first level administration code and country to build full name
	//   - latitude and longitude for scoring.
	// Also, slect only cities with a population higher than 5000 inhabitants.
	City
	.find({ "ascii": { $regex: new RegExp(req.query.q, 'i') } }, '-_id name ascii admin1 country lat long')
	.where('population').gt(5000)
	.lean()
	.exec((err, cities) => {
		
		// Note:
		// It is possible to add properties to the variable cities because the lean()
		// mongoose function send it as a plain js objects.

		// In case of an error during database query, send a 500 - InternalServerError code 
		// along with the default response.
		if (err) {
			log.error(err);
			res.send(500, responseBody);
			return next();
		}

		// If no cities match, send 404 - NotFound with default response.
		if (cities.length == 0) {
			res.send(404, responseBody);
			return next();
		}

		// Browse all matching cities
		var len = cities.length;
		while(len--) {
			var city = cities[len];

			// Add score for the current city
			utils.scoreCity(city, req.query);

			// While looping, build a disambiguous name for the city
			let state = utils.getStateCode(city.admin1, city.country);
			let countryName = utils.getCountryName(city.country);
			city.name = city.ascii + ', ' + state + ', ' + countryName;

			// Format response properties name
			city.latitude = city.lat;
			city.longitude = city.long;

			// Delete the unecessary fields
			// See ../notes.txt for comments
			delete city.ascii;
			delete city.lat;
			delete city.long;
			delete city.admin1;
			delete city.country;	
		}

		// Sort by cities descending score
		cities.sort((a, b) => { return a.score < b.score });
			
		// Page the list according to a limit if necessary
		// Not implemented

		res.send({'suggestions': cities});
		next();
	});
});
