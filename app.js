var _ = require('lodash');
var http = require('http');
var host = process.env.HOST || '0.0.0.0';
var port = process.env.PORT || 2345;
var data = require('./data.js');

var input_filepath = './data/cities_canada-usa.tsv';
data.readData(input_filepath).on("end", function() {
    console.log("Finished loading country data");
});

/**
 * Calculates the distance between two points
 * @param {decimal} lat1 - latitude of point one
 * @param {decimal} lon1 - longitude of point one
 * @param {decimal} lat2 - latitude of point two
 * @param {decimal} lon2 - longitude of point two
 * @source http://stackoverflow.com/questions/27928/how-do-i-calculate-distance-between-two-latitude-longitude-points
 */
function distance(lat1, lon1, lat2, lon2) {
    var R = 6371;
    var a = 
        0.5 - Math.cos((lat2 - lat1) * Math.PI / 180)/2 + 
        Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) * 
        (1 - Math.cos((lon2 - lon1) * Math.PI / 180))/2;
    return R * 2 * Math.asin(Math.sqrt(a));
}

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

    var point = null;
    if(!_.isUndefined(params.latitude) && !_.isUndefined(params.longitude)) {
        var lat = parseFloat(params.latitude),
            lon = parseFloat(params.longitude);
        if(_.isNumber(lat) && _.isNumber(lon)) {
            point = {
                latitude: params.latitude,
                longitude: params.longitude
            };
        }
    }

    var results = data.search(q);
    var max = 0;
    if(!_.isNull(point)) {
        _.each(results, function(res) {
            res.distance = distance(
                point.latitude, point.longitude,
                res.latitude, res.longitude);
            if(res.distance > max) {
                max = res.distance;
            }
        });
    }

    results = _.map(results, function(res) {
        var ret = {
            name: res.name + ', ' + res.country,
            latitude: res.latitude,
            longitude: res.longitude,
        };
        if(max > 0 && _.has(res, 'distance')) {
            ret.score = res.distance / max;
        }
        return ret;
    });

    return _.sortBy(results, function(res) {
        // sort by score otehrwise by name
        if(_.has(res, 'score')) {
            return -res.score;
        }
        return res.name;
    });
}

module.exports = http.createServer(function (req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
        var parsed_url = require('url').parse(req.url, true);
        var suggestions = getSuggestions(parsed_url.query);

        var status_code = 200;
        if(_.isEmpty(suggestions)) {
            status_code = 404;
        }
        res.writeHead(status_code, {'Content-Type': 'application/json'});

        res.end(JSON.stringify({
            suggestions: suggestions
        }));
    } else {
        res.writeHead(404, {'Content-Type': 'text/plain'});
        res.end();
    }
}).listen(port, host);

console.log('Server running at http://%s:%d/suggestions', host, port);
