const SuggestionsService = require('./suggestionsService');
const HttpErrors = require('http-errors');

module.exports = class searchController {

	constructor() {
		this.service = new SuggestionsService();
	}

	get(req, res) {

		// required inputs
		const cityName = req.query.q;
		if (!cityName) {
			throw new HttpErrors.BadRequest('Missing required q query param to indicate the city you are looking for');
		}

		// optional inputs
		const latitude = parseFloat(req.query.latitude);
		const longitude = parseFloat(req.query.longitude);
		const maximumSuggestions = parseFloat(req.query.m);
		const distanceScoreWeight = parseFloat(req.query.dsw);

		// get the suggestions, in case there is no we respond status 404
		const suggestions = this.service.getCitySuggestions(cityName, maximumSuggestions, latitude, longitude, distanceScoreWeight);

		return res.status(suggestions && suggestions.length > 0 ? 200 : 404).json({
			suggestions
		});

	}

}
