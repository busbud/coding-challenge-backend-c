'use strict';

var express = require('express'),
    app = express(),
    async = require('async'),
    mongoose = require('mongoose'),
    Schema = mongoose.Schema,
    Levenshtein = require('levenshtein'),
    haversine = require('haversine'),
    port = process.env.PORT || 2345;

//
app.get('/suggestions', function(req, res) {
    var q = req.query.q,
        lat = req.query.latitude,
        lon = req.query.longitude,
        suggestions = [];

    res.status(suggestions.length ? 404 : 200).type('json').send({suggestions: suggestions});
});

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
            lon: Number
        },
        country: String,
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
                        lon: values[5]
                    },
                    country: values[8],
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
};