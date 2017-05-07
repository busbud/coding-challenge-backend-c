'use strict';
/**
 * seed.js
 * ------------------------------
 * Seeding TSV data through Mongoose's models
 */
const citiesDataFilePathTSV = './data/cities_canada-usa.tsv';

var _ = require('lodash'),
	Promise = require('bluebird'),
	fs = require('fs'),
	parser = require('csv-parse/lib/sync'),
	mongoose = require('mongoose'),
	City = mongoose.model('CitySchema');

/**
* Synchronously seeds the database from TSV data
* @return {Promise}
*/
module.exports = function() {
 	return new Promise(function (resolve, reject) {
		City.findAsync()
		.then(results => {
			if (results.length > 0) {
				return resolve();
			}
			console.log('Reading input file...');
			var data = fs.readFileSync(citiesDataFilePathTSV, "utf8")
			console.log('File read.');
			var records = parser(data, {columns: true, delimiter: '\t', quote: '$', auto_parse: true});
			async.each(records, function(city, cb) {
				City.create(
					_.extend({
						latLng : {
							type: "Point",
							coordinates: [city.long, city.lat]
						}
					}, city), function(err) {
						if(err) console.log(err);
						cb();
					});
			}, function(err){
				if(err) reject(err);
				resolve();
			});
		}).catch(reject);
	});
};