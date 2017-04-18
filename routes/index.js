'use strict'

/**
 * Module Dependencies
 */
const 
	path  = require('path'),
	score = require('string-score'),
	geodist = require('geodist'),
	GeoPoint = require('geopoint'),
	utils = require('../lib/utils');

/**
 * Model Schema
 */
const City = require(path.join(__basedir, MODELS_DIR)).City;


/**
* Endpoints
*/
server.get('/suggestions', (req, res, next) => {
	
	var nameQueryString = req.query.q;
	var limit = Number(req.query.limit) || 10;
	var lat = Number(req.query.latitude) || null;
	var lon = Number(req.query.longitude) || null;

	City
	.find({ "name": new RegExp(nameQueryString, 'i') }, '-_id name admin1 country lat long')
	.where('population').gt(5000)
	.lean()
	.exec((err, cities) => {
		
		let responseBody = {'suggestions': [] };

		if (err) {
			log.error(err);
			res.send(500, responseBody);
			return next();
		}

		if (cities.length == 0) {
			res.send(404, responseBody);
			return next();
		}

		// Note: possible to add a property because they are plain js objects thanks to lean()
		var len = cities.length;
		while(len--) {
			let city = cities[len];

			//1. Calculate score
			//1.1 String score
			city.score = score(city.name, nameQueryString);

			//1.2 Distance penalty
			if (lat != null && lon != null) {
				let querystringLocation = { lat: lat, lon: lon };
				let cityLocation = { lat: city.lat, lon: city.long }
				let geodistance = geodist(querystringLocation, cityLocation, { unit: 'km' });
				city.geodistance = geodistance;
				// TODO figure out a way to build a relevant modification of the score based on the distance
			}

			// Add the full city name while looping
			let state = utils.getStateCode(city.admin1, city.country);
			let countryName = utils.getCountryName(city.country);
			city.fullname = city.name + ', ' + state + ', ' + countryName;

			// Delete the unecessary fields
			delete city.name;
			delete city.admin1;
			delete city.country;	
		}

		//2. Sort by descending score
		cities.sort((a, b) => { return a.score < b.score });
			
		//3. Truncate if necessary

		let suggestions = cities;
		res.send({'suggestions': suggestions});
		next();
	});
});