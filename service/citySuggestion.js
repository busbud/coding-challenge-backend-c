const songScore = require('./songScore');
const { distanceCalculator } = require('./distanceCalculator');

module.exports = {
	async suggestCities(query, longitude = null, latitude = null, client) {
		const suggestionList = [];
		const isLongLatPresent = longitude !== null || latitude !== null;
		const longitudeValue = longitude ? Number(longitude) : 0;
		const latitudeValue = latitude ? Number(latitude) : 0;

		const searchResult = await client.getCities(query);
		searchResult.geonames.forEach((result) => {
			if (isLongLatPresent) {
				result.dist = distanceCalculator(longitudeValue, latitudeValue, result.lng, result.lat);
			} else {
				result.dist = Infinity;
			}
			suggestionList.push(result);
		});

		return this.getScoredSuggestions(suggestionList);
	},

	async getScoredSuggestions(suggestionList) {
		let scoredList;
		if (suggestionList.length !== 1) {
			scoredList = songScore.calculateScore(suggestionList);
		} else {
			suggestionList[0].score = 1.0;
			scoredList = suggestionList;
		}

		return scoredList.map((item) => ({
			name: `${item.name}, ${item.adminCodes1.ISO3166_2}, ${item.countryName}`,
			latitude: item.lat,
			longitude: item.lng,
			score: item.score,
		}));
	},
};
