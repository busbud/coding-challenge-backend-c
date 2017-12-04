const City = require('../models/City');
const CityMatch = require('../models/CityMatch');
const geodist = require('geodist');
const request = require("request");
const unzip = require("unzip");

const fs = require("fs");
const es = require("event-stream");
const removeDiacritics = require('diacritics').remove;

require('string_score');

/**
 * Private function used to load data into a City object
 * @param datas
 * @returns {*}
 */
function getCityfromData(datas) {
    if(datas.length > 17) {
        let currentCity = new City({
            id: datas[0],
            name: datas[1],
            normalizedName: removeDiacritics(datas[1]).toLowerCase(),
            latitude: datas[4],
            longitude: datas[5],
            country: datas[8],
            population: datas[14],
            tz: datas[17]
        });
        if (currentCity.country !== undefined && (currentCity.country == 'US' || currentCity.country == 'CA')) {
            return currentCity;
        }
    }
    return null;

}

/**
 * Load all the cities from a local file into the global variable "cities"
 * This method is intended to be synchronous in order to have cities loaded before server startup
 * Use of a global variable is not a problem because we only want to have read access on this variable in the other parts of the app
 */
exports.initCities = function() {
    fs.readFileSync('data/cities5000.txt').toString().split('\n').forEach(function (line) {
        let city = getCityfromData(line.split("\t"));
        if(city != null) {
            global.cities.push(city);
        }
    });
}



/**
 * Load all the cities into a global variable "cities" from remote source
 * Use of a global variable is not a problem because we only want to have read access on this variable in the other parts of the app
 */
exports.loadCities = function() {

    let newCities = [];

    // Retrieve cities list, asynchronously
    request('http://download.geonames.org/export/dump/cities5000.zip')
        .on("error", function() {
            console.log("Remote server unreachable");
        })
        .pipe(unzip.Parse())
        .on('entry', function(entry) {
            var fileName = entry.path;
            var type = entry.type; // 'Directory' or 'File'
            var size = entry.size;

            if (fileName === "cities5000.txt") {
                entry.pipe(es.split("\n"))
                    .pipe(es.mapSync(function(data) {
                        let city = getCityfromData(data.split("\t"));
                        if(city != null) {
                            newCities.push(city);
                        }
                    }));
            } else {
                entry.autodrain();
            }
        })
        .on("error", function() {
            console.log("error during city file loading");
        })
        .on("close", function() {
            global.cities = newCities;
        });

}

/**
 * Retrieve all the cities that match 'partialCityToFind' within atg least the score of 'minimumScore'
 * @param partialCityToFind
 * @param longitudeRef
 * @param latitudeRef
 * @param minimumScore required to accept result
 * @returns {Array}
 */
exports.findCities = function(partialCityToFind, longitudeRef, latitudeRef, minimumScore) {
    const minScore = (minimumScore !== undefined && minimumScore <= 1 && minimumScore  >= 0)?minimumScore:0.1;
    let results = [];
    let distance = 0;
    if(partialCityToFind !== undefined && partialCityToFind.trim().length > 0) {
        let partialCityToFindNormalized = removeDiacritics(partialCityToFind.trim()).toLowerCase();
        for(var i = 0; i< global.cities.length;i++) {
            let currentCity = global.cities[i];

            let score =currentCity.normalizedName.score(partialCityToFindNormalized);
            if(score >= minScore) {
                distance = (longitudeRef === undefined ||latitudeRef === undefined)?0:geodist({lat: latitudeRef, lon: longitudeRef}, {lat: currentCity.latitude, lon: currentCity.longitude}, {unit: "km"});
                results.push(new CityMatch(currentCity, score, distance));
            }
        }
    }
    return results;
}