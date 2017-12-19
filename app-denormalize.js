'use strict';
var fs = require('fs');
var indexer = require('./lib/indexer');

const sourceFile = "data/cities5000.txt";
const dataFile = "data/data";

/**
 * Special entry of application that parses and denormalize TSV Cities data
 * Creates a file specified by datafile
 */
indexer(sourceFile).then(function done([lines, data]) {
	var mapStats = data.map.map(entry => Object.keys(entry).length);
	console.log("map has %s entries sharded into %s groups of %s max size",
		mapStats.reduce((sum, i) => sum + i, 0),
		data.map.length,
		mapStats.reduce((a, b) => Math.max(a, b), 0));

	console.log("data has %s entries", data.data.length);

	// It was also possible to use JSONstream and write streams here, but with a big object and 2 properties, it may be not worth it. (Or maybe just if I wanted to GZip it)
	fs.writeFile(dataFile + ".json", JSON.stringify(data), function (err) {
		console.log((err) ? "failed!" : "done!");
		process.exit();
	})
}, function fail(err) {
	console.log("Failed! " + err);
});
