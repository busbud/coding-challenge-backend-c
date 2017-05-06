'use strict';
/**
 * index.server.routes.js
 * ------------------------------
 * Setup the routes & middlewares
 */
var routes = {
    suggestions: require('./api/suggestions.server.routes.api')
};

var middlewares = {
	initApiHandlers: require('./middlewares/initApiHandlers'),
	requestValidator: require('./middlewares/requestValidator')
};
// Setup Route Bindings
module.exports = function(app) {
	app.use(middlewares.initApiHandlers);

	app.get('/suggestions', middlewares.requestValidator.validateQueryParams, routes.suggestions.get);

	app.get('*', function(req, res){
		return res.apiNotFound();
	})
};