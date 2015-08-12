var _ = require('lodash');
var http = require('http');
var host = process.env.HOST || '127.0.0.1';
var port = process.env.PORT || 2345;
var data = require('./data.js');

var input_filepath = './data/cities_canada-usa.tsv';
data.readData(input_filepath).on("end", function() {
    console.log("Finished loading country data");
});

/**
 * Gets the suggestions from the database and returns an array of dictionaries with scores
 * @param {Object} params - query params
 */
function getSuggestions(params) {
    if(_.isUndefined(params.q)) {
        return [];
    }
    var q = _.trim(params.q);
    if(_.isEmpty(q)) {
        return [];
    }

    var results = data.search(q);
    return results;
}

module.exports = http.createServer(function (req, res) {

    if (req.url.indexOf('/suggestions') === 0) {
        res.writeHead(200, {'Content-Type': 'application/json'});

        var parsed_url = require('url').parse(req.url, true);
        res.end(JSON.stringify({
            suggestions: getSuggestions(parsed_url.query)
        }));
    } else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
}).listen(port, host);

console.log('Server running at http://%s:%d/suggestions', host, port);
