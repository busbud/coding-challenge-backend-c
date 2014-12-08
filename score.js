//Assigns a score to a possible match
var geo = require('geolib');
var MAX_DISTANCE = 10000*1000; //m //Apparently, Vancouver-Cancun is a 6300 km drive
var MAX_POPULATION=5000000;// NYC is at ~8m in db
exports.MAX_DISTANCE=MAX_DISTANCE;
exports.MAX_POPULATION=MAX_POPULATION;

exports.getScore = function (city_hit, search_term, input_geo_coord) {//Calculate a score from 0 (worst) to 1 (best)
	var city=city_hit.city;
    //Currently all hit strings are equally 'similar' to search_term, ie their initial substring is a match
    //TODO: Weight on match_type (primary, ascii, alternate)
    return 1*normScoreOnCoord(input_geo_coord,{latitude: city.lat, longitude: city.long})*
    	normScoreOnPopulation(city.population); //Alternately a 'combined grade' approach would allow different weightings on different factors
};

function normScoreOnCoord(input_geo_coord, city_geo_coord) {//Returns score from 0 to 1
	if (!input_geo_coord||!input_geo_coord.latitude||!input_geo_coord.longitude) {
		return 1; //I guess? //Could also 'sanity-test' lat & long
	}
    var distance = geo.getDistance(input_geo_coord, city_geo_coord);//m
    var score = clamp((MAX_DISTANCE - distance) / MAX_DISTANCE); //No particular reason for this to be linear
    return score;
}
exports._normScoreOnCoord=normScoreOnCoord;
function normScoreOnPopulation(population) {//Returns score from 0 to 1
	var score=clamp(population/MAX_POPULATION); //The form here could be tweaked - maybe sublinear
	return score;
}
exports._normScoreOnPopulation=normScoreOnPopulation;
function clamp(val) {
	return Math.max(0,Math.min(1,val));
}