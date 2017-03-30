function SuggestionsController(options, dependencies) {

	if(!dependencies.locationsDal) {
        throw new Error("Missing injected dependency locationsDal");
    }

    if(!dependencies.suggestionsFormatter) {
        throw new Error("Missing injected dependency suggestionsFormatter");
    }
	
	this.searchLocations = function(req, res, next) {

		var query = {
			q: req.query.q || "",
			longitude: req.query.longitude,
			latitude: req.query.latitude
		};

		dependencies.locationsDal.getSuggestions(query, function(err, result) {
			var statusCode = 200;
			if(err) {
				return next(err);
			}
			if(result.hits.total === 0) {
				statusCode = 404;
			}
			res.status(statusCode).json(dependencies.suggestionsFormatter.esToHttpOutput(result));
		})
	};

	return this;
};

module.exports = SuggestionsController;