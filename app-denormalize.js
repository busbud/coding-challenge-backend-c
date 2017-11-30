'use strict';
var fs = require('fs');
var es = require("event-stream");
var through = require("through");
var tsvParser = require("./parser");

const sourceFile = "data/cities_canada-usa.tsv";
const dataFile = "data/data";

var mapForCities = [];
var dataForCities = [];

var lines = 0;

tsvParser(sourceFile)
	.pipe(through(function write(entry) { // use of through() instead of on("data"... => https://github.com/substack/stream-handbook
		if (entry) {
			checkAmbiguousEntries(entry);

			tokenizeNames((token) => addToMap(token, entry.id), entry.ascii, ...entry.altNames);

			dataForCities[entry.id] = entry;

			lines++;
		}
	}, function end() {
		console.log("Loaded %s lines", lines);

		dataForCities.forEach(handleNameCollisions);

		var mapStats = mapForCities.map(entry => Object.keys(entry).length);
		console.log("map has %s entries sharded into %s groups of %s max size",
			mapStats.reduce((sum, i) => sum + i, 0),
			mapForCities.length,
			mapStats.reduce((a, b) => Math.max(a, b), 0));
		console.log("data has %s entries", dataForCities.length);

		var data = { data: dataForCities, map: mapForCities };

		// It was also possible to use JSONstream and write streams here, but with a big object and 2 properties, it may be not worth it. (Or maybe just if wanted to GZip it)
		fs.writeFile(dataFile + ".json", JSON.stringify(data), function (err) {
			console.log((err) ? "failed!" : "done!");
			process.exit();
		})
	}));

/** Index creation ***************************************************************/
// returns the string and all possible prefixes
// from https://stackoverflow.com/questions/25788081/how-to-split-string-with-subtotal-prefix
var tokenizeNames = function (callback, ...names) {
	names.forEach((name) => name
		.split('')
		.forEach((_, index, segments) => callback(segments.slice(0, index + 1).join(''))));
}

var addToMap = function (str, id) {
	// data is sharded by string length => reduces complexity for the search by key
	var group = mapForCities[str.length];
	if (!group) group = mapForCities[str.length] = {}; // create entry if not exist

	// data is then indexed by key
	var map = group[str];
	if (!map) map = mapForCities[str.length][str] = []; // create entry if not exist
	if (map.indexOf(id) === -1) map.push(id); // avoid duplicates
}

/** Disambiguation ***************************************************************/
// TODO check for optimizations
var ambiguousNamesMap = {};
var checkAmbiguousEntries = function (entry) {
	var ambiguousEntry = ambiguousNamesMap[entry.name];
	if (ambiguousEntry) {
		entry.isAmbiguous = true;
		ambiguousEntry.isAmbiguous = true;
	} else {
		ambiguousNamesMap[entry.name] = entry;
	}
}

var handleNameCollisions = function (entry) {
	if (entry.isAmbiguous) {
		console.log("Name collision on " + entry.name);
		entry.name = entry.disambiguationName;
	}
	// not sure of the cost of delete here
	delete entry.disambiguationName;
	delete entry.isAmbiguous;
}