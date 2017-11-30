var fs = require('fs');
var through = require("through");
var tsvParser = require("./parser.js");

const sourceFile = "data/cities_canada-usa.tsv";
const dataFile = "data/data";

var mapForCities = [];
var dataForCities = [];

var lines = 0;

tsvParser(sourceFile)
	.pipe(es.mapSync(disambiguateEntry))
	.pipe(through(function write(entry) { // use of through() instead of on("data"... => https://github.com/substack/stream-handbook
		// Not sure if more piping here would be efficient
		tokenizeNames(entry);
		dataForCities[entry.id] = entry;
		lines++;
	}, function end() {
		console.log("Loaded %s lines", lines);

		dataForCities.forEach(handleNameCollisions);

		var mapStats = mapForCities.map(entry => Object.keys(entry).length);
		console.log("map has %s entries sharded into %s groups of %s max size", 
			mapStats.reduce((sum, i) => sum+i, 0),
			mapForCities.length,
			mapStats.reduce((a, b) => Math.max(a, b), 0));
		console.log("data has %s entries", dataForCities.length);
		
		var data = { data : dataForCities, map : mapForCities};
		// It was also possible to use JSONstream and write streams here, but I'm a bit tired after all the work...
		Promise.all([
			writeFile(dataFile + ".json", JSON.stringify(data)),
			writeFile(dataFile + ".beautified.json", JSON.stringify(data, null, "	")),
		]).then(function () {
			console.log("done!");
			process.exit();
		}, function () {
			console.log("failed!");
			process.exit();
		});
	}));

/** Index creation ***************************************************************/ 
var tokenizeNames = function (entry) {
	splitter(entry.ascii, token => addToMap(token, entry));
	entry.altNames
		.forEach(name => splitter(name, token => addToMap(token, entry)));
}

// returns the string and all possible prefixes
// https://stackoverflow.com/questions/25788081/how-to-split-string-with-subtotal-prefix
var splitter = function (str, callback) {
	str
		.split('')
		.forEach(function (segment, index, segments) {
			callback(segments.slice(0, index + 1).join(''));
		});
}

var addToMap = function (str, entry) {
	// data is sharded by string length => reduces complexity for the search by key
	var group = mapForCities[str.length];
	if (!group) group = mapForCities[str.length] = {}; // create entry if not exist

	// data is then indexed by key
	var map = group[str];
	if (!map) map = mapForCities[str.length][str] = []; // create entry if not exist
	if (map.indexOf(entry.id) === -1) map.push(entry.id); // avoid duplicates
}

/** Disambiguation ***************************************************************/ 
// TODO check for optimizations
var ambiguousNamesMap = {};
var disambiguateEntry = function (entry) {
    var ambiguousEntry = ambiguousNamesMap[entry.name];
    if (ambiguousEntry) {
        entry.isAmbiguous = true;
        ambiguousEntry.isAmbiguous = true;
    } else {
        ambiguousNamesMap[entry.name] = entry;
    }
    return entry;
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

/** File Writer ***************************************************************/ 
var writeFile = function (path, data) {
	return new Promise(function (resolve, reject) {
		fs.writeFile(path, data, function (err) {
			if (err) reject(err);
			else resolve(data);
		});
	})
}