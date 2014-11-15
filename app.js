var http = require('http');
var fs = require('fs');
var stream = require('stream');
var bh = require(__dirname + '/backend_helper.js');
var querystring = require('querystring');
var port = process.env.PORT || 2345;
var DATA_FILE_PATH = __dirname + '/data/cities_canada-usa.tsv';

module.exports = http.createServer(function (req, res) {
  
  var suggestions = [];
  var success = false;

  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions?') === 0) {
  	var args = querystring.parse(req.url.substring(13));

  	fs.readFile(DATA_FILE_PATH, 'utf8', function(err, data) {
  		// Get city matches
  		var cities = bh.getMatches(data.split('\n'), args.q);
  		
  		// Make suggestions based on matches and query input
  		suggestions = bh.makeSuggestions(cities, args.q, args.longitude, args.latitude);

  		if(suggestions.length > 0) {
  			res.writeHead(200);
  			success = true;
  		}
  		res.end(JSON.stringify({
  			success: success,
  			suggestions: suggestions
  		}, null, 2));
  	});
  } else {
    res.end("You are probably looking for the Busbud suggestions endpoint. Try appending '/suggestions' to the base url.");
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);