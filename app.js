var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var host = process.env.YOUR_HOST || '0.0.0.0';

var util = require('./util.js');

module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {

    var params = url.parse(req.url, true).query;

    util.getFipsCodes('data/admin1_codes.tsv', function(fips_codes) {

        util.findSuggestions('data/cities_canada-usa.tsv', params.q, params.latitude, params.longitude, fips_codes, function(suggestions) {

            if (suggestions.length > 0) res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' });
            else res.writeHead(404, {'Content-Type': 'application/json'});

            res.end(JSON.stringify({
                suggestions: suggestions
            }));
        });
    });

  } else {
    res.end();
  }
}).listen(port, host);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);