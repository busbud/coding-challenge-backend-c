var http = require('http');
var fs = require('fs');
var stream = require('stream');
var bh = require(__dirname + '/backend_helper.js');
var querystring = require('querystring');
var port = process.env.PORT || 2345;
var DATA_FILE_PATH = __dirname + '/data/cities_canada-usa.tsv';

module.exports = http.createServer(function (req, res) {
  
  var suggestions = [];

  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
  	var query_args = querystring.parse(req.url.substring(13));
  	var q = query_args.q;
  	var lon = query_args.longitude;
  	var lat = query_args.latitude;

  	fs.readFile(DATA_FILE_PATH, 'utf8', function(err, data) {
  		var cities = data.split('\n');
  		
  		// Split the lines of the data file
  		// Then filter out the cities with population less than 5000
  		// as well as cities that do not match the input string
  		cities = cities.map(function(line) {
  			return line.split('\t');
  		}).filter(function(city) {
  			return parseInt(city[14]) >= 5000 && city[1].indexOf(q) === 0;
  		});
  		
  		// Make suggestions
  		suggestions = cities.map(function(city) {
  			return {
  				name: city[1],
  				longitude: city[4],
  				latitude: city[5],
  				score: bh.getScore(city[1], city[4], city[5], q, lon, lat)
  			};
  		});

  		// Sort suggestions by score
  		suggestions = suggestions.sort(function(a,b) {
  			return b.score - a.score;
  		});

  		if(suggestions.length > 0)
  			res.writeHead(200);
  		res.end(JSON.stringify({
  			suggestions: suggestions
  		}));
  	});
  	// var stream = fs.createReadStream(DATA_FILE_PATH);
  	// stream.pipe(res, {end : false});
  	// stream.on('end', function() {
  	// 	console.log(res);
  	// });
  } else {
  	res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end(JSON.stringify({
    	status: 'bad endpoint',
    	suggestions: suggestions
    }));
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

var f = function(res) {

};

// 1. Read URL, 
// 		If it contains 'suggestions/' then parse query
//			If parsed query is empty then return empty suggestions, statusCode = 404
//			Else Search (filtering out cities less than 5000 population) the file
//				 Compute scores
//				 Sort by score
//				 Set the suggestions to the sorted list
//				 Set success flag
//			Send the stringified JSON output, statusCode = 200
//		Else statusCode = 404 
//			 Send empty response, set failed flag