const express = require('express');
const SuggestionsController = require('./suggestionsController');

module.exports = class App {
	constructor() {
		this.server = express();
		this.bootstrap();
	}

	start() {
		return this.server.listen(process.env.PORT || 2345);
	}

	stop() {
		return this.server.close();
	}

	// bootstrap initialize all the routes and error handlers
	bootstrap() {

		// suggestion api
		this.server.get('/suggestions', function(req, res) {
			return new SuggestionsController().get(req,res);
		});

		// error handler
		// we need to use the 4th parameter here
		// to make express understand that is our errors handler
		this.server.use(function(err, req, res, next) {

			// we only expose and log server errors
			if (!err.message || !err.status || err.status >= 500) {
				console.error(err);
				return res.status(500).json({
					error: 'Something went wrong'
				});
			}

			return res.status(err.status).json({
				error: err.message
			});
		});
	}
}
