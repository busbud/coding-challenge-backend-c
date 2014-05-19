var express = require('express');
var engine  = require(__dirname + '/engine');

// Environment Variables
var port     = process.env.PORT     || 2345;
var resource = process.env.RESOURCE || '/suggestions';

// Instantiate Express Service
var service  = express();

// Express Route - Application Endpoint
service.get(resource, function(request, response) {

    // CORS Headers
    response.header('Access-Control-Allow-Origin',  '*');
    response.header('Access-Control-Allow-Headers', 'X-Requested-With');
    response.set({'content-type': 'application/json; charset=utf-8'});

    var query     = request.query.q;
    var longitude = request.query.longitude;
    var latitude  = request.query.latitude;
    if (query) {
        if ('undefined' == typeof longitude || (longitude >= -180 && longitude <= 180)) {
            if ('undefined' == typeof latitude || (latitude >= -90 && latitude <= 90)) {
                engine.search(query, longitude, latitude, function(error, results) {
                    var responseCode = 200;
                    if (!results.length) {
                        responseCode = 404;
                    }

                    results = {suggestions: results};
                    response.send(results, responseCode);
                });

            } else {
                response.send({'message':'Latitude parameter is wrong'}, 400);
            }

        } else {
            response.send({'message':'Longitude parameter is wrong'}, 400);
        }

    } else {
        response.send({'message':'Required parameter q is missing'}, 400);
    }

});

function initialize(callback) {
    engine.initialize(function(error, response) {
        if (error) {
            callback(error);
        } else {
            service.listen(process.env.PORT, function(error) {
                callback(error, response);
            });
        }

    });
}

module.exports.app        = service;
module.exports.initialize = initialize;