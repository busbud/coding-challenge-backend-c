'use strict';

var path = require('path');
var util = require('./util');

/**
 * Region singleton. 
 * Contained all information in memory.
 * Used mainly for performance purpose.
 * @constructor {object} options options
 * @Returns {Region}
 */
var Region = function Region (options){

	var self = this;
	self.name = options.name || ''; //Can be the name of a country/region for example.
	self.options = options = options || {}; 
	self.cities = {};       // Main object 
	self.thr = 0.5;         // Threshold used for shrinking all irrelevant results

	//Fetch all information form file and put it in the region object.
	util.getInfoFromFile(path.join(__dirname,'../data/cities_canada-usa.tsv'), function(err, cities){

		if(err){
			throw new Error('Need to fetch data before receiving request');
		}
		self.cities = cities;
	});
	return self;
};

/**
* Get the suggestion for a given cities object
* @export
* @param {string} query_str
* @param {string} lat Latitude
* @param {string} lng Longitude
* @param {function} cb Callback function
*/
Region.prototype.getSuggestions = function(query_str, lat, lng, cb){

	var cities = this.cities;
	var suggestions = [];

	var lat_n = parseFloat(lat);
	var lng_n = parseFloat(lng);

	//Loop on the whole array of cities.
	for (var i = 0; i < cities.length; i++) {

	    var city = cities[i];
	    
	    if (city.population >= 5000) {

	    	//Get the score by string distance algorithm
	        var score = util.computeScoreCity(query_str.toLowerCase(), city.name.toLowerCase());
	        
	        if (score > this.thr) {

	        	//If lat and lng are not provide (or incorrect) no need to go through the next step.
	            if (lat !== undefined && lng !== undefined && util.isLatitude(lat_n) && util.isLongitude(lng_n)) {

	                var arg1 = {
	                	latitude: lat,
	                	longitude: lng 
	                };

	                var arg2 = {
	                	latitude: city.lat,
	                	longitude: city.long
	                };

	                try {

	                	var scoreGeoloc = util.computeScoreGeoloc(arg1, arg2);

	                	//Shrink results below a certain score because geoloc weigh more next.
	                	if(scoreGeoloc > 0.4){
	                		score = parseFloat((score * 0.4) + (scoreGeoloc * 0.6)).toFixed(2);
	                	}

	                } catch (err){

	                	//1. Don't mind of the error since we provide an answer with 'relevant' score anyway
	                	//Or
	                	//2. Handle error or at least notifyng client of the wrong format. 
	                }
	            } else {
	                score = parseFloat(score).toFixed(2);
	            }

	            if (score > this.thr) { 

	                suggestions.push({
	                	name: util.prettyPrintCity(city),
	                	latitude: city.lat,
	                	longitude: city.long,
	                	score: score,
	                });
	            }
	        }
	    }
	}

	//Sort suggestion by score before callback.
	suggestions = util.sortByScore(suggestions);

	//Trim the final array to max 20 results.
	//This should be set by an conf variable
	var max_result = 20;
	suggestions.splice(max_result);

	return cb({suggestions : suggestions});
}

module.exports = Region;