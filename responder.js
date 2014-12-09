//Receives url.parse() objects, queries the search structure, assigns a score, and returns a response object
var _=require('lodash');
var score=require('./score');
var formatter=require('./cityToJSON');

var MAX_SUGGESTIONS=15;

exports.getResponse = function (url_obj,search_structure) {//Returns response with response.suggestions : [response_hit objects] (of form response_hit.city (city object), response_hit.score)
	var search_term=url_obj.query.q;
	if (!search_term) {
		return [];//I guess?
	}
	var matches=search_structure.getMatches(search_term.toLowerCase());
	var coord={latitude:url_obj.query.latitude, longitude:url_obj.query.longitude};
	return {suggestions: _(matches).
		map(function(match) {
			return {city : match.city, score : score.getScore(match,search_term, coord)};
		}).
		sortBy(function(suggestion) {
			return -suggestion.score;
		}).
		first(MAX_SUGGESTIONS).
		value()};
};
exports.getFormattedResponse = function (url_obj,search_structure) {
	var formattedResponse=exports.getResponse(url_obj,search_structure);
	formattedResponse.suggestions=_(formattedResponse.suggestions).map(function(response_hit) {
		return formatter.getFormattedObj(response_hit);
	}).value();
	return formattedResponse;
};
exports.getResponseString = function (url_obj,search_structure) {
	var formattedResponse=exports.getFormattedResponse(url_obj,search_structure);
	return JSON.stringify(formattedResponse);
};