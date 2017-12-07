var http = require('http');
var url = require('url');
var fs = require('fs');

var GeoNames = require('./lib/geonames');
var GeoMinLn = 2;
var GeoMaxRs = 10;

var host = '0.0.0.0';
var port = process.env.PORT || 2345;
var endp = '/suggestions';

var server = http.createServer();

/**
 * Registering a callback when a request ccure
 */
server.on('request', function (request, response) {
    response.setHeader('Access-Control-Allow-Origin', '*');
    
    // some error handling on request, response errors
    request.on('error', function (err) {
        console.error(err);
        response.statusCode = 400;
        response.setHeader('Content-Type', 'text/plain');
        response.end('400 Bad Request');
    });
    response.on('error', function (err) {
        console.error(err);
    });

    // check fot method and url path
    // Send the demo index.html file in the public folder make a live search via a web browser
    if (request.method === 'GET' && request.url === '/') {
        response.statusCode = 200;
        response.setHeader('Content-Type', 'text/html');
        response.end(fs.readFileSync(__dirname + '/public/index.html'));
        return;
    }

    // check fot method and url path
    // Respect the API design of /suggestins
    if (request.method === 'GET' && request.url.indexOf(endp) === 0) {
        var up = url.parse(request.url, true);
        var qs = up.query.q;
        var la = up.query.latitude;
        var lo = up.query.longitude;

        if ('undefined' === typeof qs) {
            response.statusCode = 400;
            response.setHeader('Content-Type', 'text/plain');
            response.end('400 Bad Request');
            return;
        }

        response.statusCode = 200;
        response.setHeader('Content-Type', 'application/json');
        
        //execute the search function of the GeoNames module and return the result as JSON Content-Type
        if (('undefined' !== typeof la) && ('undefined' !== typeof lo)) {
            response.end(JSON.stringify(gn.searchCity(qs, {latitude: la, longitude: lo})));
        } else {
            response.end(JSON.stringify(gn.searchCity(qs)));
        }
    } else if (request.method === 'GET') {
        response.statusCode = 404;
        response.setHeader('Content-Type', 'text/plain');
        response.end('404 Not Found');
    } else {
        response.statusCode = 405;
        response.setHeader('Content-Type', 'text/plain');
        response.end('405 Method Not Allowed');
    }

});

/**
 * Load the GeoNames data file and start teh API endpoint when loding has finished
 */
var gn = new GeoNames('./data/cities_canada-usa.tsv', GeoMinLn, GeoMaxRs, function (geonames) {
    server.listen(port, host, function () {
        console.log('Server running at http://' + host + ':' + port + '/.');
    });
});
