	module.exports = function() {
	var _ = require('lodash'),
	fs = require('fs'),
	csv = require('csv-streamify'),
	parser = csv({ objectMode: true, columns: true, delimiter: '\t', quote: 'none' });

	var mongoose = require('mongoose');
	var City = mongoose.model('CitySchema');

	parser.on('data', function (city) {
		City.create(_.extend({latLng : {
			type: "Point",
			coordinates: [ city.long, city.lat ]
		}}, city), function(err){
			if(err) console.log(err);
		});
	});

	City.find().exec(function(err, results){
		if (err) throw err;
		if (results.length < 1) {
			console.log('Seeding cities...');
			fs.createReadStream('./data/cities_canada-usa.tsv').pipe(parser);
		}
	});
}

