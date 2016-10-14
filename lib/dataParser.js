var path = require('path');
var fs = require('fs');
var d3 = require('d3');
var util = require('./util');

/**
 * Load all information in memory. Used mainly for performance purpose.
 * @constructor {object} options options
 * @Returns {Data}
 */
var DataParser = function DataParser (options){

	var self = this;
	self.cities = {};

	// Read tsv file
	fs.readFile(path.join(__dirname,'../data/cities_canada-usa.tsv'),'utf8', function(err, data){
		if(err){
				throw new Error('Need to fetch data before receiving request');
		}
		else {
			var startTime = new Date().getTime();
			console.log('start loading data');

			//Only load relevant data performance purpose.
			self.cities  = d3.tsv.parse(data, function(d) {
				return {
					id : +d.id,
					name : d.name,
					state: d.country === 'US' ? d.admin1 : util.getCanadianProvinceIsoCode(+d.admin1),
					latitude : +d.lat,
					longitude : +d.long,
					country : d.country,
					population : +d.population,
				};
			}
		);
		var endTime = new Date().getTime();
		var executionTime = endTime - startTime;
		console.log('end loading data. %d cities loaded into memory after %d milliseconds.', self.cities.length, executionTime);
		}
	});

	return self;
};

module.exports = DataParser;
