/**
 * Gets the application data and stores it into an sqlite3 database
 */
var fs = require("fs");
var es = require("event-stream");
var NodeCache = require( "node-cache" );
var cache = new NodeCache({ stdTTL: 60 * 30});
var _ = require('lodash');

var DATA = [];

/**
 * Parses a line from the database
 * @param {array} data - to be parsed 
 * @warning internal function, should not be called alone
 */
function parseLine(data) {
    var id = data[0],
        name = _.trim(data[1]),
        alt_names = data[3],
        lat = data[4],
        longi = data[5],
        country = data[8];

    // make alt_names an array
    if(!_.isUndefined(alt_names)) {
        // remove empty alt_names
        alt_names = _.filter(alt_names.split(','), function(alt) {
            return !_.isEmpty(_.trim(alt));
        });
    } else {
        alt_names = [];
    }

    // insert values
    return {
        geonames_id: id,
        name: name,
        alt_names: alt_names,
        latitude: lat,
        longitude: longi,
        country: country
    };
}

/** 
 * reads data from the file and calls parseLine to format the data
 * @param {string} filepath - url of file to parse
 */
function readData(filepath) {
    console.log('parsing file data');
    // read file
    return fs.createReadStream(filepath)
        // split lines
        .pipe(es.split("\n"))
        // split tabs into array
        .pipe(es.mapSync(function(data) {
            return data.split("\t");
        }))
        // parse data
        .pipe(es.mapSync(function(data) {
            DATA.push(parseLine(data));
        }));
}

/**
 * Searches data for name
 * @param {string} name - name to search for, can be a partial name
 * @returns {array} - results of dictonary values matching search
 */
function search(name) {
    // lowercase name
    name = name.toLowerCase();

    // check the cache
    var cached = cache.get(name);
    if(!_.isUndefined(cached)) {
        return cached;
    }

    // get values
    var results = [];
    _.each(DATA, function(data) {
        if(_.startsWith(data.name.toLowerCase(), name)) {
            results.push(data);
        } else {
            var matched = false; // only do it once
            _.each(data.alt_names, function(alt) {
                if(!matched && _.startsWith(alt.toLowerCase(), name)) {
                    results.push(data);
                    matched = true;
                }
            });
        }
    });

    // store in cache and return
    cache.set(name, results);
    return results;
}

// module exports
module.exports = {
    readData: readData,
    search: search
};
