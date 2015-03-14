var csv = require('csv');
var fs = require('fs');
var _ = require('underscore');

var cityIDs = [];
var lats = {};
var longs = {};
var cities = {};

// Reads a given tsv file and pulls out relevant attributes.
// Passes these attributes to populate to populate the arrays
exports.updateRecords = function(file) {

	fs.readFile(file, function(err, data) {
		_.each(data.toString().split('\n'), function(a) {
			var parts = a.split('\t');
			var id = parts[0];
			var cityName = parts[2];
			var lat = parts[4];
			var lon = parts[5];
			var state = parts[10];
			var country = parts[8];

			// Change to full country names and add Canadian provinces
			if (country === 'CA') {
				country = 'Canada';
				state = provinceFromCode(state);
			} else if (country === 'US') {
				country = 'United States';
			}

			var cityDetails = cityName + ", " + state + ", " + country;
			populate(cityName, cityDetails, lat, lon, id);
		});
	});
};

// Helper method to updateRecords, populates the arrays
var populate = function(cityName, cityDetails, latitude, longitude, id) {
	cityName = String(cityName).toLowerCase();
	var toAdd = {city: cityName, id: id};
	cityIDs.push(toAdd);
	cities[id] = cityDetails;
	lats[id] = latitude;
	longs[id] = longitude;
};

// convert 'admin1' codes to province initials for canadian cities
var provinceFromCode = function(code) {
	switch(+code) {
		case 1: return "AB";
		case 2: return "BC";
		case 3: return "MB";
		case 4: return "NB";
		case 5: return "NL";
		case 7: return "NS";
		case 8: return "ON";
		case 9: return "PE";
		case 10: return "QC";
		case 11: return "SK";
		case 12: return "YT";
		case 13: return "NT";
		case 14: return "NU";
	}

	return "Invalid code";
}

// Testing function to check population of key value arrays
exports.getMatch = function(name) {
	var matches = []
	for (var i = 0; i < cityIDs.length; i++) {
		if (cityIDs[i].city === name) {
			matches.push({
				name: cities[cityIDs[i].id],
				longitude: longs[cityIDs[i].id],
				lattitude: lats[cityIDs[i].id],
			});
		}
	}
	return matches;
}