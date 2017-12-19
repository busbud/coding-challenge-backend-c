'use strict';
var es = require("event-stream");
var JSONStream = require('JSONStream');
var sort = require('sort-stream'); // Nice to have: check perf on this one
var DB = require('./database');

const DEBUG = process.env.DEBUG;

var db = new DB();

/** Retrieval ***************************************************************/

/**
 * Gets the matching alternate name when relevant
 * 
 * @param {City} entry City object to be searched
 * @param {string} query The initial query parameter
 * @returns {string} The alternate name matching the query
 */
function getAlias(entry, query) {
	if (!entry.ascii.startsWith(query) && !entry.name.toLowerCase().startsWith(query)) { // More case escaping could be done here
		return entry.altNames.find(alt => alt.startsWith(query))
	}
}

/**
 * Retrieves the city with the matching alias if any, and the score for sorting
 * 
 * @param {string} query The initial query parameter
 * @param {function} score The function to compute score
 * @returns {object} Tuple with the city, the matched alias and the score
 */
function retrieveData(query, score) {
	return index => {
		var entry = db.get(index);
		var alias = getAlias(entry, query);

		return [entry, alias, score(alias ? alias : entry.ascii, entry.latlon)];
	}
}

/** Sorting ***************************************************************/
/**
 * Sort method to sort cities by score
 * Sorting from : https://stackoverflow.com/questions/34956869/sorting-a-data-stream-before-writing-to-file-in-nodejs
 * Changed -1 & +1 to make it descending
 * 
 * @param {float} x Score of the first city
 * @param {float} y Score of the second city
 * @returns {int} 1, 0 or -1 depending on the relative score of each city
 */
function cmp([, , x], [, , y]) {
	return x < y ? 1 : x === y ? 0 : -1;
}

/** Response formatting ***************************************************************/
/**
 * Formats the name for display
 * 
 * @param {city} item City object to be displayed
 * @param {string} alias Alias to be displayed if the match has been done by alias
 * @param {float} score Score to be displayed
 * @returns {object} City object ready to be displayed
 */
function formatProperty([item, alias, score]) {
	var name = (alias) ? alias + " => " + item.name : item.name;

	return { "name": name, "latitude": item.latlon[0], "longitude": item.latlon[1], "score": Math.round(score * 10) / 10 };
}

/**
 * Formats the name for display with debug values
 * 
 * @param {city} item City object to be displayed
 * @param {string} alias Alias to be displayed if the match has been done by alias
 * @param {float} score Score to be displayed
 * @returns {object} City object ready to be displayed
 */
function formatPropertyDebug([item, alias, score]) {
	var result = formatProperty([item, alias, score]);
	result.ascii = item.ascii;
	result.altNames = item.altNames;

	return result;
}

/** Scoring ***************************************************************/
// Note that the score is poorly implemented
// A Levenshtein here doesn't help because it would only return the length of the missing part of the match, which is already known by getting name length.
// Only optimization on score is that it is computed with the current item only. A scoring involving other results would be more costly.

/**
 * Returns the function that will compute score
 * 
 * @param {string} query Original query parameter
 * @param {float} lat Latitude
 * @param {float} lon Longitude
 * @return {function} Function to compute score
 */
function getScoreFn(query, lat, lon) {
	
	return (lat && lon && !isNaN(lat) && !isNaN(lon))
		? (name, latlon) => getStringDistance(query, name) * getDistanceScore([lat, lon], latlon)
		: (name) => getStringDistance(query, name);
}

/**
 * Returns the distance between strings
 * 
 * @param {string} query Original query parameter
 * @param {string} name Name of the city, or alias when the match is an alternate name
 * @return {int} Score representing the distance between strings
 */
function getStringDistance(query, name) {

	return (name.startsWith(query + " ")) 
		? 1 // special case for multi word names // Check /suggestions?q=oro
	 	: query.length / name.length;
}

/**
 * Returns a value representing the distance between points
 * 
 * @param {float} lat0 First latitude
 * @param {float} lon0 First longitude
 * @param {float} lat1 Second latitude
 * @param {float} lon1 Second longitude
 * @return {float} Value representing the geographical distance (this is not the actual distance! It is just a number representing it)
 */
// from: http://jonisalonen.com/2014/computing-distance-between-coordinates-can-be-simple-and-fast/
// Note: this is an approximation, so it will not be right for 'far from user' and 'close to each other' places, but, this is just for scoring, so the error seems tolerable
function getDistance([lat0, lon0], [lat1, lon1]) {
	var x = lat0 - lat1
	var y = (lon0 - lon1) * Math.cos(lon1)
	//	return 110.25 * Math.sqrt(x * x + y * y); // We don't need 110.25 * Math.sqrt() because we don't compute an absolute distance. We just want to have an int representing distance

	return (x * x) + (y * y);
}

/**
 * Returns a score corresponding to the geographical distance
 * 
 * @param {object} latlon0 First lat/lon object
 * @param {object} latlon1 Second lat/lon object
 * @return {float} Value representing the geographical scoring
 */
function getDistanceScore(latlon0, latlon1) {
	var dist = getDistance(latlon0, latlon1);
	// TODO check the distances those number represent. For now, it is just for demo
	if (dist < 1) return 1; // not far
	if (dist < 9) return 0.8; // far
	if (dist < 49) return 0.5; // further
	if (dist < 100) return 0.3; // even further
	else return 0.1; // in a galaxy, far far away
}

/**
 * Search the prefix in the data
 * 
 * @param {string} query Query parameter to be completed
 * @param {function} lat Optional latitude parameter as a function
 * @param {function} lon Optional longitude parameter as a function
 * @returns {Stream} Stream for response
 */
module.exports = function (query, lat, lon) {
	var found = db.find(query);

	if (found) {
		var score = getScoreFn(query, lat(), lon()); // TODO scoring is poorly implemented
		return es.readArray(found)
			.pipe(es.mapSync(retrieveData(query, score)))
			.pipe(sort(cmp))
			.pipe(es.mapSync(DEBUG ? formatPropertyDebug : formatProperty)) // Sync or we may loose order
			.pipe(JSONStream.stringify('{"suggestions": [', ',', ']}')) // TODO compare performance of stringify VS string concat
	}
}
