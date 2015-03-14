var csv = require('csv');
var fs = require('fs');
var _ = require('underscore');
var scorer = require('./scoring');
var compare = require('./comparisons');

var cityIDs = [];
var lats = {};
var longs = {};
var cities = {};
var populations = {};

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
			var population = parseInt(parts[14]);

			// Change to full country names and add Canadian provinces
			if (country === 'CA') {
				country = 'Canada';
				state = provinceFromCode(state);
			} else if (country === 'US') {
				country = 'United States';
			}

			var cityDetails = cityName + ", " + state + ", " + country;
			populate(cityName, cityDetails, lon, lat, id, population);
		});

		cityIDs.sort(compare.cities);
	});
};

// Helper method to updateRecords, populates the arrays
var populate = function(cityName, cityDetails, longitude, latitude, id, population) {
	cityName = String(cityName).toLowerCase();
	var toAdd = {city: cityName, id: id};
	cityIDs.push(toAdd);
	cities[id] = cityDetails;
	lats[id] = latitude;
	longs[id] = longitude;
	populations[id] = population;
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
};

// Returns a list of matches based on prefix entered
exports.getMatches = function(name, longitude, latitude) {
	var matches = []
	var i = findFirstIndex(name);

	// Keep traversing cityID array until city names no longer match
	while (cityIDs[i].city.slice(0, name.length) === name) {
		matches.push({
			name: cities[cityIDs[i].id],
			longitude: longs[cityIDs[i].id],
			latitude: lats[cityIDs[i].id],
			score: scorer.getScore(populations[cityIDs[i].id], longs[cityIDs[i].id], 
									lats[cityIDs[i].id], longitude, latitude),
		});
		i++;
	}

	matches.sort(compare.scores);
	return matches;
};

// Uses a binary search to find the first matching city
// from the sorted array cityIDs
var findFirstIndex = function(prefix) {
	var minIndex = 0;
	var maxIndex = cityIDs.length-1;
	var currentIndex;
	var currentCity;

	// Find any matching city
	while (minIndex <= maxIndex) {
		currentIndex = (minIndex + maxIndex) / 2 | 0;
		currentCity = cityIDs[currentIndex].city;
		if (currentCity.slice(0, prefix.length) < prefix) {
			minIndex = currentIndex + 1;
		} else if (currentCity.slice(0, prefix.length) > prefix) {
			maxIndex = currentIndex - 1;
		} else {
			break;
		}
	}

	// Traverse to first matching city
	while(true) {
		if (cityIDs[currentIndex - 1].city.slice(0, prefix.length) === prefix) {
			currentIndex--;
		} else {
			break;
		}
	}
	return currentIndex;

};













