var http = require('http');
var fs = require('fs');
var stream = require('stream');
var es = require('event-stream');
var bh = require(__dirname + '/backend_helper.js');
var inspect = require('util').inspect;
var querystring = require('querystring');
var port = process.env.PORT || 2345;
var DATA_FILE_PATH = __dirname + '/data/cities_canada-usa.tsv';

module.exports = http.createServer(function (req, res) {

  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions?') === 0) {
  	var suggestions = [];
    var success = false;
    var data_file = fs.createReadStream(DATA_FILE_PATH);
    var args = querystring.parse(req.url.substring(13));

    data_file.on('data', function(data) {
      suggestions = suggestions.concat(
        bh.makeSuggestions(bh.getMatches(data.toString('utf8').split('\n'), args.q), args.q, args.longitude, args.latitude)
      );
    });

    data_file.on('end', function() {
      if(suggestions.length > 0) {
        res.writeHead(200);
        success = true;
      } else {
        res.writeHead(404);
      }

      res.end(JSON.stringify({ success: success, suggestions: suggestions }, null, 2));
    });

  } else {
    res.end("You are probably looking for the Busbud suggestions endpoint. "
    	+ "Try appending '/suggestions' to the base url, and provide a parameter 'q' in the query.");
  }
}).listen(port);