// Setup app
var express = require('express');
var validator = require('express-validator');
var autocomplete = require('./autocomplete');

var app = express();
var port = process.env.PORT || 2345;

app.use(validator());

// Home
app.get('/', function(req, res) {
	res.json({
    app: 'Busbud Autocomplete',
    author: 'Christophe Naud-Dulude'
  });
});

// Suggestions API
app.get('/suggestions', function(req, res) {
	req.assert('q', 'q parameter is required and must be a string').notEmpty().matches(/([A-Za-z\-]+)/);
	if(req.query.latitude != undefined) {
		req.assert('latitude', 'latitude parameter must be numeric').isFloat();
	}
	if(req.query.longitude != undefined) {
		req.assert('longitude', 'longitude parameter must be numeric').isFloat();
	}

	var errors = req.validationErrors(true);
	if (errors) {
    res.status(400).json({
			errors: errors,
			suggestions: []
		});
    return;
  }

	var q = req.query.q
	var lon = req.query.longitude
	var lat = req.query.latitude

	autocomplete.search(q, function(cities) {
		// Give score

		// Format result json

		res.json({
			suggestions: cities
		});
	});
});

// The private key is used as a really basic security system and is mostly used for debugging.
// This is not production ready but this way people hitting the URL without the
// key won't be able to delete the database.
const privateKey = 'bb4af96c181317bed81ee6c61a70c23e';

app.post('/suggestions/populate', function(req, res) {
	req.assert('key', 'Permission denied').notEmpty().equals(privateKey);
	var errors = req.validationErrors(true);
	if (errors) {
		res.status(403).json({
			errors: errors,
		});
		return;
	}

	autocomplete.populate();
	res.json({
		message: 'Populated database successfully'
	})
})

app.post('/suggestions/clear', function(req, res) {
	req.assert('key', 'Permission denied').notEmpty().equals(privateKey);
	var errors = req.validationErrors(true);
	if (errors) {
		res.status(403).json({
			errors: errors,
		});
		return;
	}
	
	autocomplete.clear();
	res.json({
		message: 'Flushed database successfully'
	})
})

// Run app
app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
