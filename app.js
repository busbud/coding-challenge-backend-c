// Setup
var express = require('express');
var app = express();
//var url = require('url');
var port = process.env.PORT || 2345;


var data = require('./data');
data.updateRecords('data/cities_canada-usa.tsv');

// Suggestions
app.get('/suggestions', function(req, res){
  var q = req.query.q;
  var lon = req.query.longitude;
  var lat = req.query.latitude;
  var lim = req.query.limit;
  var matches = data.getMatches(q.toLowerCase(), lon, lat, lim);
  if (matches.length > 0)
		res.end(JSON.stringify({ suggestions: matches }, null, 2));
	else
		res.status(404).send('No matches found');
});


app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);