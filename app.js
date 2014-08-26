// Setup app
var express = require('express');
var app = express();

var port = process.env.PORT || 2345;

// Home
app.get('/', function(req, res) {
	res.json({
    app: 'Busbud Autocomplete',
    author: 'Christophe Naud-Dulude'
  });
});

// Suggestions API
app.get('/suggestions', function(req, res) {
	res.json({
    suggestions: []
  });
});

// Run app
app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
