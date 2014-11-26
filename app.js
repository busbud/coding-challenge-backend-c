var http = require('http');
var fs = require('fs');
var stream = require('stream');
var es = require('event-stream');
var bh = require(__dirname + '/backend_helper.js');
var inspect = require('util').inspect;
var querystring = require('querystring');
var port = process.env.PORT || 2345;
var data = require(__dirname + '/data/data.js');

module.exports = http.createServer(function (req, res) {

  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions?') === 0) {
  	var suggestions = [];
    var success = false;
    var args = querystring.parse(req.url.substring(13));

    suggestions = bh.makeSuggestions(data.cities, args.q, args.longitude, args.latitude);

    if(suggestions.length > 0) {
      res.writeHead(200);
      success = true;
    }

    res.end(JSON.stringify({ success: success, suggestions: suggestions }, null, 2));

  } else {
    res.end("You are probably looking for the Busbud suggestions endpoint. "
    	+ "Try appending '/suggestions' to the base url, and provide a parameter 'q' in the query.");
  }
}).listen(port);

console.log("Server now running at port " + port);