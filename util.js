var fs = require('fs');
var d3 = require('d3');
var geolib = require('geolib');

/**
 * Find suggestions among all cities from file
 * @param filename
 * @param _city
 * @param _lat
 * @param _lng
 * @param lookup
 * @param cb
 */
exports.findSuggestions = function (filename, _city, _lat, _lng, lookup, cb) {

    var suggestions = [];

    fs.readFile(filename, 'utf8', function (err, data) {

        //Verify if file found
        if (data !== undefined && data !== 'undefined') {

            var cities = d3.tsv.parse(data);
            var threshold = 0.4;    //Threshold for global score

            for (var i = 0; i < cities.length; i++) {
                city = cities[i];

                //Verify population
                if (city.population > 5000) {

                    var score = 0.0;
                    var scoreCity = 1 - (levenshteinDistance(_city.toLowerCase(), city.name.toLowerCase()) * 0.1);
                    var scoreDistance = 0.0;

                    if (scoreCity > threshold) {

                        //Optional
                        if (_lat != undefined && _lng != undefined && _lat.length > 0 && _lng.length > 0) {

                            //Get distance between two coordinates (lat, long)
                            var d = geolib.getDistance({ 'latitude': _lat, 'longitude': _lng }, { 'latitude': city.lat, 'longitude': city.long });
                            scoreDistance = getDistanceScore(d);
                            score = parseFloat((scoreCity * 0.2) + (scoreDistance * 0.8)).toFixed(1);
                        }
                        else {
                            score = parseFloat(scoreCity).toFixed(1);
                        }

                        if (score > threshold) {

                            var key = city.country + '.' + city.admin1;
                            var admin_div = lookup[key];
                            suggestions.push(new Suggestion(city.name + ', ' + admin_div.name + ', ' + city.country, city.lat, city.long, score));
                        }
                    }
                }
            }
        }

        return cb(suggestions.sort(compare));
    });
}

/**
 * Get FIPS codes and create a lookup table for each city
 * @param filename
 * @param cb
 */
exports.getFipsCodes = function(filename, cb) {
    fs.readFile(filename, 'utf8', function (err, data) {

        var codes = d3.tsv.parse(data);

        //Create lookup table for Administrative divisions
        var lookup = {};
        for (var i = 0, len = codes.length; i < len; i++) {
            lookup[codes[i].fips_gn] = codes[i];
        }

        return cb(lookup);
    });
}

/**
 * Return difference between to sequences
 * @param a
 * @param b
 * @returns {*}
 */
var levenshteinDistance = function(a, b) {
    if (a.length == 0) return b.length;
    if (b.length == 0) return a.length;

    var matrix = [];

    var i;
    for (i = 0; i <= b.length; i++) {
        matrix[i] = [i];
    }

    var j;
    for (j = 0; j <= a.length; j++) {
        matrix[0][j] = j;
    }

    for (i = 1; i <= b.length; i++) {
        for (j = 1; j <= a.length; j++) {
            if (b.charAt(i - 1) == a.charAt(j - 1)) {
                matrix[i][j] = matrix[i - 1][j - 1];
            } else {
                matrix[i][j] = Math.min(matrix[i - 1][j - 1] + 1,
                    Math.min(matrix[i][j - 1] + 1,
                        matrix[i - 1][j] + 1));
            }
        }
    }

    return matrix[b.length][a.length];
}

/**
 * Return score according to distance
 * @param distance
 * @returns {number}
 */
var getDistanceScore = function(distance) {

    if (distance <= 100000) return 1.0;
    else if (distance > 100000 && distance <= 200000) return 0.9;
    else if (distance > 200000 && distance <= 300000) return 0.8;
    else if (distance > 300000 && distance <= 400000) return 0.7;
    else if (distance > 400000 && distance <= 500000) return 0.6;
    else if (distance > 500000 && distance <= 600000) return 0.5;
    else if (distance > 600000 && distance <= 700000) return 0.4;
    else if (distance > 700000 && distance <= 800000) return 0.3;
    else if (distance > 800000 && distance <= 900000) return 0.2;
    else return 0.1;
}

/**
 * Sort descending
 * @param a
 * @param b
 * @returns {number}
 */
var compare = function(a,b) {
    if (a.score > b.score)
        return -1;
    if (a.score < b.score)
        return 1;
    return 0;
}

function Suggestion(name, latitude, longitude, score) {
    this.name  = name;
    this.latitude = latitude;
    this.longitude = longitude;
    this.score = score;
}