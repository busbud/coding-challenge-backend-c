const http = require('http');
const express = require('express');
const bunyan = require('bunyan');
const middleware = require('./middleware');
const client = require('./client/geoNamesClient');
const service = require('./service/citySuggestion');
const routes = require('./routes');

const port = process.env.PORT || 2345;

async function startServer(app) {
	app.log = bunyan.createLogger({
		name: 'Busbud',
		level: process.env.LOGGER || 'info',
	});

	app.server = http.createServer(app);

	app.log.info('starting server on %d', port);

	middleware.registerMiddleware(app);
	routes(app, service, client);
	middleware.registerErrorHandlers(app);

	app.server.listen(
		port,
	);

	return app;
}

startServer(express())
	.catch((error) => {
		// eslint-disable-next-line no-console
		console.error('Error starting application.', error);
	});
