'use strict';

var express = require('express'),
    app = express(),
    async = require('async'),
    _ = require('lodash'),
    mongoose = require('mongoose'),
    Schema = mongoose.Schema,
    Levenshtein = require('levenshtein'),
    haversine = require('haversine'),
    port = process.env.PORT || 2345;

//
app.get('/suggestions', function(req, res) {
    var q = req.query.q,
        lat = req.query.latitude,
        lng = req.query.longitude,
        City = mongoose.model('City'),
        suggestions = [];

    City.find({$or: [{name: {'$regex': new RegExp("^.*"+q+".*","gi")}}, {asciiname: {'$regex': new RegExp("^.*"+q+".*","gi")}}]}, 'name asciiname coord admin1').lean().exec(function(err, cities) {
        async.map(cities, function(city, cb) {
            var suggestion = {
                name: city.name,
                latitude: city.coord.lat,
                longitude: city.coord.lng,
                score: Math.min(ldist(q, city.name), ldist(q, city.asciiname))
            };
            cb(null, suggestion);
        }, function(err, results) {
            results.sort(compareScore);
            _.forEach(results, function(r, err) {
                suggestions.push(r);
            });

            if (err) res.status(500).type('json').send({suggestions: []});
            else if (!suggestions.length) res.status(404).type('json').send({suggestions: []});
            else res.status(200).type('json').send({suggestions: suggestions});
        });
    });

    //http://maps.googleapis.com/maps/api/geocode/json?latlng=42.98339,-81.23304&sensor=true
});

function compareScore(a,b) {
  if (a.score < b.score) return -1;
  if (a.score > b.score) return 1;
  return 0;
}

function ldist(q, cityName) {
    var l = new Levenshtein(q,cityName);
    return l.distance;
};

mongoose.connect('mongodb://localhost/busbud', function(err) {
    if (err) return console.log(err);
    else console.log('Connected to MongoDB');
    app.listen(port, '127.0.0.1', function() {
        console.log('Server running at http://127.0.0.1:%d/suggestions', port);
    });

    loadModels();
    //populateDB();
});

function loadModels() {
    var CitySchema = new Schema({
        _id: false,
        id: Number,
        name: String,
        asciiname: String,
        altnames: String,
        coord: {
            lat: Number,
            lng: Number
        },
        country: String,
        admin1: String,
        population: Number,
        tz: String,
        modified: String
    });

    mongoose.model('City', CitySchema);
    console.log('Models loaded');
};

function populateDB() {
    var fs = require('fs'),
        path = require('path');

    // Adapted Keegan Parker's script to work with MongoDB
    fs.readFile(path.join('data', 'cities_canada-usa.tsv'), function(err, data) {
        if (err) return console.error(err);
        else console.log('Populating MongoDB...');

        data = data.toString();

        // Skip first line
        var index = data.indexOf('\n');
        data = data.substring(index + 1);
        while (data.indexOf('\n') > -1) {
            var line_index = data.indexOf('\n');
            var line = data.substring(0, line_index);
            var values = [];

            // Push all tab-separated value into the values array
            while (line.indexOf('\t') > -1) {
                var index = line.indexOf('\t'),
                    value = line.substring(0, index);
                values.push(value);
                line = line.substring(index + 1);
            }
            values.push(line);

            // Turn empty strings in to null
            values = values.map(function(value) {
                if (!value.length) return null;
                return value;
            });

            // Adding each city to DB
            if (values[14] > 5000) {
                var City = mongoose.model('City');
                var city = new City({
                    _id: values[0],
                    name: values[1],
                    asciiname: values[2],
                    altnames: values[3],
                    coord: {
                        lat: values[4],
                        lng: values[5]
                    },
                    country: values[8],
                    admin1: sanitizeAdminCodes(values[10]),
                    population: values[14],
                    tz: values[17],
                    modified: values[18]
                });

                city.save(function (err, city) {
                    console.log(err || city.name + ' added succesfully');
                });
            };
            data = data.substring(line_index + 1);
        };
    });

    function sanitizeAdminCodes(adminCode) {
        var adminCodeMap = {
            '01': 'AB',
            '02': 'BC',
            '03': 'MB',
            '04': 'NB',
            '05': 'NL',
            '07': 'NS',
            '08': 'ON',
            '09': 'PE',
            '10': 'QC',
            '11': 'MB',
            '12': 'YT',
            '13': 'NT',
            '14': 'NU'
        };
        return parseInt(adminCode) ? adminCode[parseInt(adminCode)] : 'US';
    };
};