var _ = require('lodash'),
	fs = require('fs'),
	csv = require('fast-csv'),
	Promise = require('bluebird'),
	mongoose = require('mongoose'),
	City = mongoose.model('CitySchema')

module.exports = function() {
 	return new Promise(function (resolve, reject) {
		City.findAsync()
		.then(function(results) {
			if (results.length > 0) {
				return resolve();
			}
			console.log('Seeding cities...');
			var stream = fs.createReadStream('./data/cities_canada-usa.tsv');
			csv
			 .fromStream(stream, {
			 	headers : true,
			 	objectMode: true,
			 	ignoreEmpty: true,
			 	delimiter: '\t',
			 	quote: null
			 }).transform(function(city, next){
			     City.createAsync(_.extend({latLng : {
						type: "Point",
						coordinates: [city.long, city.lat]
					}}, city)
			     ).catch(function(err){
			     	console.log(err);
			     })
			 }).on("end", function(){
			 	console.log('Done seeding.');
			 	resolve();
			 });
		}).catch(reject);
    });
}

