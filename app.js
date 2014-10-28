var http = require('http');
var express = require('express');

var cities = require('./cities');

var port = process.env.PORT || 2345;

// Express setup
var app = express();

app.get('/suggestions', function(req, res) {
    var q = req.query.q;
    var latitude = req.query.latitude;
    var longitude = req.query.longitude;

    cities.search(q, latitude, longitude, function(cities) {
        res.status(cities.length === 0 ? 404 : 200)
            .json({suggestions: cities});
    });
});

// Basic error handling
app.use(function(err, req, res, next){
  console.error(err.stack);
  res.status(500).json({'message': 'An internal error has occurred.'});
});

// Load the cities and start the server when it's done
cities.loadFile('./data/cities_canada-usa.tsv', function(err) {
    if (err) {
        console.error('Could not load cities:\n%s', err.stack);
    } else {
        app.listen(port, function() {
            var address = this.address();
            console.log('Server running at http://%s:%d/suggestions', address.address, address.port);
        });
    }
});

module.exports = app;