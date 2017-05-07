'use strict';
/**
 * Module dependencies.
 */

var routes = {
    suggestions: require('./api/suggestions.server.routes.api')
};

var middlewares = {
	initApiHandlers: require('./middlewares/initApiHandlers'),
	requestValidator: require('./middlewares/requestValidator')
};

/**
* Set routes and middlewares
*
* @param {Object} app - express app
*/

module.exports = function(app) {
	app.use(middlewares.initApiHandlers);

	app.get('/suggestions', middlewares.requestValidator.validateQueryParams,
		routes.suggestions.get);

	app.get('*', function(req, res){
		return res.apiNotFound();
	})
};