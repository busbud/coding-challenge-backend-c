const CitiesDataGateway = require('./citiesDataGateway');
const geolib = require('geolib');
const stringScore = require('string-score');
const getProvinceFromId = require('./canadianProvinceHelperService').provinceFromId;

// please consider only getCitySuggestions() as a public method of this class
module.exports = class SuggestionsService {

	constructor() {
		// the object that we use to access the row data
		this.gateway = new CitiesDataGateway();

		this.defaultDistanceWeight = 0.5;
		this.defaultMaximumSuggestions = 50;
	}

	/*
		getCitySuggestions() provides an autocomplete service based on a city name
	 		returns an array of cities : [ { name, score, latitude, longitude }, ... ]

		Required parameter is the cityName only.

		Optional parameters are:
		  - maximumSuggestions, integer, default is 50
		  - latitude and longitude: float
			- distanceScoreWeight: float, between 0 and 1, default is 0.5,
														 which add more or less importance on the distance
														 to the lat and long provided

		Note that if latitude and longitude are note provided, the distanceScoreWeight will be set to 0

	*/
	getCitySuggestions(cityName, maximumSuggestions, latitude, longitude, distanceScoreWeight) {

		// set maximumSuggestions to default in case not provided
		maximumSuggestions = maximumSuggestions ? maximumSuggestions : this.defaultMaximumSuggestions;

		const cities = this.gateway.all();

		const shouldHandleDistance = latitude && longitude;

		if (shouldHandleDistance) {
			// set distanceScoreWeight to default in case not provided
			distanceScoreWeight = distanceScoreWeight ? distanceScoreWeight : this.defaultDistanceWeight;
		} else {
			distanceScoreWeight = 0;
		}

		let maxDistance = 0;
		let maxScoreOnName = 0;

		// the first iteration will add a new property _smd to the city objects
		// with the distance if provided and the score on the city name
		// _smd stands for suggestions meta data
		return cities.map(c => {

			const scoreName = this.getCityNameScore(c, cityName);

			c._smd = {
				distance: shouldHandleDistance ?  this.getCityDistance(c, latitude, longitude) : null,
				scoreOnName: scoreName
			}

			if (shouldHandleDistance && c._smd.distance > maxDistance) maxDistance = c._smd.distance;
			if (c._smd.scoreOnName > maxScoreOnName) maxScoreOnName = c._smd.scoreOnName;

			return c;
		})
		// the second iteration normalize the score on the name
		// and create the score on the distance if provided
		// the create the final scoring
		.map(c => {

			if (shouldHandleDistance) {
				c._smd.scoreOnDistance = 1 - c._smd.distance / maxDistance;
			}
			c._smd.scoreOnName = c._smd.scoreOnName / maxScoreOnName;

			if (c._smd.scoreOnName > 0) {
				if (c._smd.scoreOnDistance) {
					c._smd.finalScore = (1 - distanceScoreWeight) * c._smd.scoreOnName + distanceScoreWeight * c._smd.scoreOnDistance;
				} else {
					c._smd.finalScore = (1 - distanceScoreWeight) * c._smd.scoreOnName;
				}
			} else {
				c._smd.finalScore = 0;
			}
			return c;
		})
		// remove 0 scores
		.filter(c => {
			return c._smd.finalScore > 0;
		})
		// order descending
		.sort((c1, c2) => {
			return c2._smd.finalScore - c1._smd.finalScore
		})
		// cut the array
		.slice(0, maximumSuggestions)
		// sanitize the output
		.map( c => {
			return {
				name: `${c.name}, ${c.country === 'CA' ? getProvinceFromId(c.admin1) : c.admin1 }, ${c.country}`,
				score: c._smd.finalScore,
				latitude: c.lat,
				longitude: c.long
			}
		});

	}

	// two helper functions to get scores

	// get a scrore between a city and a string
	// is comparing the city name for now
	getCityNameScore(city, cityName) {

		// we use a fuziness weight of 0.5
		// i played with it, it does not change so much the result
		const score = stringScore(city.name, cityName, 0.5);

		// we filter everything less than 0.2 so we dont have really bad suggestions
		return score > 0.2 ? score : 0;

	}

	// get the distance between a city and a lat and long
	getCityDistance(city, latitude, longitude) {

		if (isNaN(parseFloat(city.lat)) || isNaN(parseFloat(city.long))) return null;

		return geolib.getDistance(
	    {latitude: city.lat, longitude: city.long},
	    {latitude, longitude}
		);

	}

}
