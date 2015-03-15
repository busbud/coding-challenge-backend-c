// Setup
var express = require('express');
var app = express();
module.exports = app;

//var url = require('url');
var port = process.env.PORT || 2345;

// Render view with ejs
app.set('views', __dirname + '/views');
app.engine('html', require('ejs').renderFile);
app.use(express.static(__dirname + '/public'));

app.get('/', function(req, res) {
  res.render('index.html');
});

var data = require('./data');
data.updateRecords('data/cities_canada-usa.tsv');

// Suggestions
app.get('/suggestions', function(req, res){
  	var q = req.query.q;
  	var lon = req.query.longitude;
  	var lat = req.query.latitude;
  	var lim = req.query.limit;

  	var errorMessage = [];
	if (req.query.longitude != undefined && isNaN(parseFloat(lon))) {
  		errorMessage.push("Longitude must be a float");
  	};
  	if (req.query.latitude != undefined && isNaN(parseFloat(lat))) {
  		errorMessage.push("Latitude must be a float");
  	};
  	if (req.query.limit != undefined && isNaN(parseInt(lim))) {
  		errorMessage.push("Limit must be an integer");
  	};

  	if (errorMessage.length > 0) {
  		res.status(404).json({
			Errors: errorMessage
		});
  	} else {
  		var matches = data.getMatches(q.toLowerCase(), lon, lat, lim);
  		if (matches.length > 0) {
			res.end(JSON.stringify({ suggestions: matches }, null, 2));
	  	} else {
			res.status(404).json({
				suggestions: []
			});
		}
	}
});


app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);