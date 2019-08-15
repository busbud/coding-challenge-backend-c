// TODO: case insensitive !!!
//
// TODO: score should include population?
// TODO: why return a 404 if no matches found?
//
// TODO: calculate distance from user lat, long
// estimate is good enough
// http://jonisalonen.com/2014/computing-distance-between-coordinates-can-be-simple-and-fast/
// https://stackoverflow.com/a/15737369
// TODO: user metro area lookup? bounding box? voronoi map?
// for each city, find nearest city. bounding box vertex goes midway (prorate
// by population?) on line connecting two cities. if user lat,long are in city
// bounding box, city gets a score bump (how much?)
// TODO: partialCities has many keys -> same val; custom dict to reduce size
// https://codereview.stackexchange.com/questions/85842/a-dictionary-that-allows-multiple-keys-for-one-value
// TODO: handle typos in city query
// this is hard: https://medium.com/snipette/types-of-typos-aeee34ab0424
// most queries from phone? no keyboard clustering assumptions
// supported languages? each language has its own list of common typos
// TODO: scoring function?!? score = max(1, f(nameScore, distanceScore))
// TODO: nameScore = 1 for perfect match, 1 - n * delta
//                      where n distance from perfect match
// TODO: distance score = exp(-1 * distance from city center)
// TODO:
//
var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var fs = require('fs');
var S = require('string');

var cities = {};
var partialCities = {};

var csvData = fs.readFileSync('data/cities_canada-usa.tsv', 'utf-8');
var csvLines = csvData.split('\n');
var headers = csvLines.shift().split('\t');

function degToRadian(angle) {
    return Math.PI * angle / 180;
}

function estimateDistance(lat1, long1, lat2, long2) {
    const deglen = 111.138;
    lat1 = degToRadian(lat1);
    long1 = degToRadian(long1);
    lat2 = degToRadian(lat2);
    long2 = degToRadian(long2);
    var x = lat2 - lat1;
    var y = (long2 - long1) * Math.cos((lat1 + lat2) * 0.0087266);

    return deglen * Math.sqrt(x*x + y*y) // in kilometers
}

function addOrAppend(dict, key, val) {
    if (dict[key]) {
        dict[key].push(val);
    } else {
        dict[key] = [val];
    }
}

csvLines.forEach(function(line){
    row = line.split('\t');
    jsonObj = {};
    headers.forEach(function(key, i) {jsonObj[key] = row[i]});
    cityName = S(row[1]).latinise().toString();
    addOrAppend(cities, cityName, jsonObj);
    for (var i=1; i<cityName.length; i++) {
        addOrAppend(partialCities, cityName.substring(0,i), jsonObj);
    }
})

module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    queryParams = url.parse(req.url, true).query;
    var matches = [];
    if (queryParams.q) {
        cityQ = S(queryParams.q).latinise().toString();
        if (cities[cityQ]) {
            cities[cityQ].forEach(function(city) {
                city['score'] = 1.0
                matches.push(city)
            });
        }
        if (partialCities[cityQ]) {
            partialCities[cityQ].forEach(function(city) {
                // too rough an estimate?
                delta = Math.abs(city['ascii'].length - cityQ.length);
                city['score'] = Math.max(1.0 - delta * 0.1, 0);
                matches.push(city)
            });
        }
    }
    if (matches.length === 0) {
        res.writeHead(404, {'Content-Type': 'text/plain'});
    } else {
        if (queryParams.latitude && queryParams.longitude) {
            matches.forEach(function(match) {
                delta = estimateDistance(match.lat, match.long, queryParams.lattitude, queryParams.longitude)
                match['score'] += Math.min(50 - delta, 0) / 100 ;
                match['score'] /= 1.5;
            })
        }
        res.writeHead(200, {'Content-Type': 'text/plain'});
    }
    res.end(JSON.stringify({
        suggestions: matches
    }));
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
