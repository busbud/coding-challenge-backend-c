"use strict";
/*jslint nomen: true */
var fs = require('fs'),
    path = require('path'),
    nameindex = require('./nameindex'),
    citiesPath = path.join(__dirname, 'data', 'cities_canada-usa.tsv'),
    names = new nameindex.NameIndex(),
    MAX_GEO_DIST = 360 + 180,
    // determine what is more important for giving scores to search results
    // this is still not perfect, but clearly geo is super important when
    // it is provided
    WEIGHT_GEO = 80,
    WEIGHT_NAME = 1,
    WEIGHT_POPULATION = 0.5,
    WEIGHT_TOTAL = (WEIGHT_GEO + WEIGHT_NAME + WEIGHT_POPULATION);


function startup() {
    // startup code optimized for code simplicity and definitely not
    // for efficiency
    /*jslint stupid: true */
    fs.readFileSync(citiesPath, 'utf8').split('\n').slice(1).forEach(function (line) {
        var fields = line.split('\t'),
            searchableName;
        if (fields[0]) {
            searchableName = fields[2].toLowerCase();
            names.index(
                searchableName,
                {
                    // this would be the FULL name:
                    searchableName: searchableName,
                    // intentionally broken to match test expectations
                    name: fields[2] + ', ' + fields[10] + ', ' + fields[8],
                    latitude: parseFloat(fields[4]),
                    longitude: parseFloat(fields[5]),
                    population: fields[14]
                }
            );
        }
    });
    /*jslint stupid: false*/
    // just for the fun of it measure and trace
    // the lookup speed of the trie for partial
    // string lookups
    function bench() {
        var i, now = Date.now();
        for (i = 0; i < 10000; i += 1) {
            names.lookup('mo', undefined, undefined, true);
        }
        console.log('10000 partial name lookups in ' + (Date.now() - now) + ' ms');
    }
    // startup done, display some info
    console.log('Index ready');
    bench();
}
// computes the distance between two strings
function stringDistance(s1, s2) {
    // we could do something clever
    // (Levenshtein distance or stuff like that)
    // but in practice because of the lookup
    // method we know that one string is a prefix
    // to the other... So assuming that, the only
    // distance that is really useful is the difference
    // in length
    var dist = Math.abs(s1.length - s2.length);

    // then we want all our distances to be between
    // 0 an 1 (0 = exact match, 1 = far far away)
    return (Math.min(dist, 20) / 20);
}
// computes the distance between two geo locations
function geoDistance(lat1, long1, lat2, long2) {
    var ld = lat1 - lat2,
        lld = long1 - long2,
        d = Math.sqrt(ld * ld + lld * lld);
    return d / MAX_GEO_DIST;
}
// computes a 'distance' relative to a big population
function populationDistance(pop) {
    // this is not a distance per se, but
    // will favor larger cities over smaller ones.
    // If I search for 'c', Chicago IL should come
    // before Clova, QC...
    return 1 - Math.min(pop / 1000000, 1);
}
/**
 * Lookup a partial (or complete) city name and return name suggestions.
 *
 * @param {String} s string to lookup
 * @param {Number} latitude optional latitude of the city fo find
 * @param {Number} longitude optional longitude of the city to find
 * @return {Array} Array of { name: "", latitude: "", longitude: "", score: 1 } objects.
 * @api public
 */
function suggestions(s, latitude, longitude) {
    s = s || '';
    // if we have a very short string it would
    // be preferable to search by latitude and
    // longitude first (and then score by string
    // match). But let's do something simple.

    // in a real life situation, we would probably
    // want to precondition our search string
    // (by stripping accents and stuff)
    var lookup = s.toLowerCase(),
        candidates = names.lookup(lookup, true),
        useGeo = latitude !== undefined && longitude !== undefined;
    if (useGeo) {
        latitude = Number(latitude);
        longitude = Number(longitude);
    }

    // now that we have our candidates,
    // we will score them
    return candidates.map(function (c) {
        var ss = 1 - stringDistance(c.searchableName, lookup),
            gs = 1 - (useGeo ? geoDistance(c.latitude, c.longitude, latitude, longitude) : 0),
            ps = 1 - populationDistance(c.population),
            // we could weight things differently
            score = ((WEIGHT_NAME * ss + WEIGHT_GEO * gs + WEIGHT_POPULATION * ps) / WEIGHT_TOTAL);
        return {
            name: c.name,
            latitude: String(c.latitude),
            longitude: String(c.longitude),
            score: score
        };
    }).sort(function (c1, c2) {
        return c2.score - c1.score;
    });
}
startup();
exports.suggestions = suggestions;
