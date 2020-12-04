const bodyParser = require('body-parser');

module.exports = {
	registerMiddleware: (app) => {
		app.use(bodyParser.urlencoded({ extended: false }));
		app.use(bodyParser.json());
	},

	registerErrorHandlers: (app) => {
		app.use((err, req, res, next) => { // eslint-disable-line no-unused-vars
			if (err.output && err.output.statusCode) {
				app.log.error(err);

				return res.status(err.output.statusCode).json(err.output.payload);
			}

			app.log.error('Internal server error', req.method, req.url);

			return res.status(500).json({ error: 'Internal server error' });
		});
	},
};
