var Boom = require('boom');
var express = require('express');

var app = express();
var port = process.env.PORT || 2345;

var LocationsDAL = require('./lib/data/locations.js');
var SuggestionsController = require('./lib/controllers/suggestions.js');
var SuggestionsFormatter = require('./lib/formatters/suggestions.js');
var SuggestionsValidator = require('./lib/controllers/suggestions_validator.js');
var SuggestionsRouter = require('./routes/suggestions.js');

var locationsDal = new LocationsDAL({
	host: process.env.ES_HOST || "192.168.99.100:32771",
	index: process.env.ES_INDEX || "locations",
	cacheLength: process.env.CACHE_LENGTH,
	cacheAge: process.env.CACHE_AGE
});

var suggestionsController = new SuggestionsController({}, {
	locationsDal: locationsDal,
	suggestionsFormatter: new SuggestionsFormatter()
});

var suggestionsRouter = new SuggestionsRouter({
	suggestionsValidator: new SuggestionsValidator(),
	suggestionsController: suggestionsController
}).router;

app.use('/suggestions', suggestionsRouter);

app.use(function(err, req, res, next) {
	console.error(err);
	var statusCode = err.isBoom ? err.output.statusCode : 500;

	if(err.isBoom) {
		return res.status(statusCode).send(err);		
	}

  	return res.status(statusCode).send(Boom.wrap(err));
});

app.use(function(req, res, next) {
  	res.status(404).send(Boom.create(404, "Resource not found!"));
});

 
app.listen(port, function(err, res) {
	console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app;
