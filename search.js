'use strict';
var es = require("event-stream");
var JSONStream = require('JSONStream');
var sort = require('sort-stream'); // Nice to have: check perf on this one
var DB = require('./database');

const DEBUG = process.env.DEBUG;

var db = new DB();

module.exports = function (query, lat, lon) {
	var found = db.find(query);

	if (found) {
		var score = getScoreFn(query, lat(), lon()); // TODO scoring is poorly implemented
		return es.readArray(found)
			.pipe(es.mapSync(retrieveData(query, score)))
			.pipe(sort(cmp))
			.pipe(es.mapSync(DEBUG ? formatPropertyDebug : formatProperty)) // Sync or we may loose order
			.pipe(JSONStream.stringify('{"suggestions": [', ',', ']}'))  // TODO compare performance of stringify VS string concat
	}
}

var retrieveData = function (query, score) {
	return (index) => {
		var entry = db.get(index);
		var alias = getAlias(entry, query);
		return [entry, alias, score(alias ? alias : entry.ascii, entry.latlon)];
	}
}

var getAlias = function (entry, query) {
	if (!entry.ascii.startsWith(query) && !entry.name.toLowerCase().startsWith(query)) // More case escaping could be done here
		return entry.altNames.find(alt => alt.startsWith(query))
}

/** Sorting ***************************************************************/
// sorting from : https://stackoverflow.com/questions/34956869/sorting-a-data-stream-before-writing-to-file-in-nodejs
// changed -1 & +1 to make it descending
// x and y are scores
var cmp = ([, , x], [, , y]) => x < y ? 1 : x == y ? 0 : -1;

/** Response formatting ***************************************************************/
var formatProperty = function ([item, alias, score]) {
	var name = (alias) ? alias + " => " + item.name : item.name;
	return { "name": name, "latitude": item.latlon[0], "longitude": item.latlon[1], "score": score.toFixed(1) };
}

var formatPropertyDebug = function ([item, alias, score]) {
	var result = formatProperty(arguments);
	result.ascii = item.ascii;
	result.altNames = item.altNames;
	return result;
}

/** Scoring ***************************************************************/
// WARNING ! score is poorly computed and is inspired by these ancient fellows advices:
// - for distance: http://i0.kym-cdn.com/photos/images/facebook/000/234/146/bf8.jpg
// - for names: http://i0.kym-cdn.com/photos/images/facebook/000/234/765/b7e.jpg
// even listening those songs did not help: https://www.youtube.com/watch?v=MTQnYyyAFCo and https://www.youtube.com/watch?v=W0wPNow3ymc

// Only optimization on score is that it is computed with the current item only. A scoring involving other results would be more costly.

var getScoreFn = function (query, lat, lon) {
	return (lat && lon && !isNaN(lat) && !isNaN(lon))
		? (name, latlon) => getStringDistance(query, name) * getDistanceScore([lat, lon], latlon)
		: (name) => getStringDistance(query, name);
}

var getStringDistance = function (str, name) {
	if (name.startsWith(str + " ")) return 1; // special case for multi word names // Check /suggestions?q=oro
	return str.length / name.length;
}

// from: http://jonisalonen.com/2014/computing-distance-between-coordinates-can-be-simple-and-fast/
// Note: this is an approximation, so it will not be right for 'far from user' and 'close to each other' places, but, this is just for scoring, so the error seems tolerable
var getDistance = function ([lat0, lon0], [lat1, lon1]) {
	var x = lat0 - lat1
	var y = (lon0 - lon1) * Math.cos(lon1)
	//	return 110.25 * Math.sqrt(x * x + y * y); // We don't need 110.25 * Math.sqrt() because we don't compute an absolute distance. We just want to have an int representing distance
	return x * x + y * y;
}

var getDistanceScore = function (latlon0, latlon1) {
	var dist = getDistance(latlon0, latlon1);
	// TODO check the distances those number represent. For now, it is just for demo
	if (dist < 1) return 1; // not far
	if (dist < 9) return 0.8; // far
	if (dist < 49) return 0.5; // further
	if (dist < 100) return 0.3; // even further
	else return 0.1; // in a galaxy, far far away
}