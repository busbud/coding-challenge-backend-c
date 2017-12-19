'use strict';
var through = require('through');
var tsvParser = require('./parser');

/** Cities data array ***************************************************************/

var dataForCities = [];

/** Index of prefixes ***************************************************************/

var mapForCities = [];

/**
 * Returns the string and all possible prefixes
 * Inspired from: from https://stackoverflow.com/questions/25788081/how-to-split-string-with-subtotal-prefix
 * 
 * @param {function} callback Function that stores the prefixes
 * @param {array} names All names to be parsed. 0 is the ascii name, others are alternate names
 * @return {void} Nothing, all prefixes are passed to the callback
 */
var tokenizeNames = function (callback, ...names) {
	names.forEach(name => name
		.split('')
		.forEach((_, index, segments) => callback(segments.slice(0, index + 1).join(''))));
}

/**
 * Populates the mapForCities object store
 * 
 * @param {string} str The prefix or full name to index
 * @param {int} id The id of the city this prefix belongs to 
 * @return {void} Nothing, items are put in the mapForCities
 */
var addToMap = function (str, id) {
	// Data is sharded by string length => reduces complexity for the search by key
	var group = mapForCities[str.length];
	if (!group) { // Create entry if not exist
		group = mapForCities[str.length] = {};
	}

	// Data is then indexed by key
	var map = group[str];
	if (!map) { // Create entry if not exist
		map = mapForCities[str.length][str] = [];
	}

	if (map.indexOf(id) === -1) { // Push to index, but avoid duplicates
		map.push(id);
	}
}

/** Disambiguation ***************************************************************/
// TODO Check for optimizations. It could be also sorted to be compared. This would have been more costly, but more useful on the logging part

var ambiguousNamesMap = {};

/**
 * Function to check if a city with the same name already exists
 * If so, it flags it as ambiguous as well as the matched previous city
 * 
 * @param {object} entry City to be checked
 * @returns {void} Nothing everything is put in the ambiguousNamesMap
 */
var checkAmbiguousEntries = function (entry) {
	var ambiguousEntry = ambiguousNamesMap[entry.name];
	if (ambiguousEntry) {
		entry.isAmbiguous = true;
		ambiguousEntry.isAmbiguous = true;
	} else {
		ambiguousNamesMap[entry.name] = entry;
	}
}

/**
 * Renames city if it is ambiguous. Removes the useless properties 'disambiguationName' and 'isAmbiguous'
 * 
 * @param {object} entry City to be renamed
 * @returns {void} Nothing, the input object is modified
 */
var handleNameCollisions = function (entry) {
	if (entry.isAmbiguous) {
		console.log("Name collision on **" + entry.name + "** => replaced by **" + entry.disambiguationName + "**");
		entry.name = entry.disambiguationName;
	}
	// Deleting properties is not a good approach, but this is just a demo
	delete entry.disambiguationName;
	delete entry.isAmbiguous;
}

/**
 * Launches the parsing and indexing of the source data file
 * 
 * @param {string} sourceFile Path to the file to be parsed and indexed
 * @returns {Object} A promise that will be called with the data
 */
module.exports = function (sourceFile) {
	var lines = 0;

	return new Promise(function (done) {
		tsvParser(sourceFile)
			.pipe(through(function write(entry) { // Use of through() instead of on("data"... => https://github.com/substack/stream-handbook
				if (entry) {
					entry.id = lines;
					
					checkAmbiguousEntries(entry);

					tokenizeNames(token => addToMap(token, entry.id), entry.ascii, ...entry.altNames);

					dataForCities[entry.id] = entry;

					lines++;
				}
			}, function end() {
				console.log("Loaded %s lines", lines);

				dataForCities.forEach(handleNameCollisions);

				done([lines, { data: dataForCities, map: mapForCities }]);
			}));
	})
}
