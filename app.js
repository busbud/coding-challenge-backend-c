// TODO: score should include population?
// TODO: user metro area lookup? bounding box? voronoi map?
// for each city, find nearest city. bounding box vertex goes midway
// on line connecting two cities. if user lat,long are in city
// bounding box, city gets a score bump (how much?)
// TODO: handle typos in city query
// this is hard: https://medium.com/snipette/types-of-typos-aeee34ab0424
// most queries from phone? no keyboard clustering assumptions
// supported languages? each language has its own list of common typos
// TODO:
//
var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var fs = require('fs');
var S = require('string');

// multi key dictionary
function MKDict(hashFunc) {
    this.objStore = {};
    //key -> list of ids (use set instead)
    this.keyStore = {}
    this.hashFunc = hashFunc;
}
MKDict.prototype = {
    constructor: MKDict,
    addOrAppend: function(key, obj) {
        id = this.hashFunc(obj);
        if (!this.objStore[id]) {
            this.objStore[id] = obj;
        }
        if (this.keyStore[key]) {
            this.keyStore[key].push(id);
        } else {
            this.keyStore[key] = [id];
        }
    },
    contains: function(key){
        if (this.keyStore[key]) {
            return true;
        }
        return false;
    },
    get: function(key) {
        results = [];
        var ids = this.keyStore[key];
        for (var i=0; i<ids.length; i++) {
            id = ids[i];
            results.push(this.objStore[id]);
        }
        return results;
    }
}

// helper functions

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

function exactMatches(query, results) {
   if (cities[query]) {
       cities[query].forEach(function(city) {
           city['score'] = 1.0
           results.push(city)
       });
   }
}

function partialMatches(query, results) {
   if (partialCities.contains(query)) {
       partialCities.get(query).forEach(function(city) {
           // too rough an estimate?
           delta = Math.abs(city['ascii'].length - cityQ.length);
           city['score'] = Math.max(1.0 - delta * 0.1, 0);
           results.push(city)
       });
   }
}

function updateScoreForDistance(lat, long, matches) {
    matches.forEach(function(match) {
        delta = estimateDistance(match.lat, match.long, lat, long)
        match['score'] += Math.max(50 - delta, 0) / 100 ;
        // normalize back to 0-1
        match['score'] /= 1.5;
    })
}

// init data structures

// the headers from the data we want to include in response
var keepers = ["id", "name", "ascii", "lat", "long", "country", "admin1", "population", "tz"];
// cityname -> [jsonData...]
// eg: london -> [dataForLondonOH, dataforLondonKY, ...]
var cities = {};
// cityname.substring -> [jsonData...]
// eg: londo -> [dataforLondonOH, dataforLondonderryON, ...]
var partialCities = new MKDict(function(obj) {return obj.id});
// two letter country code + "." + two letter admin1 code -> state/province name
var stateName = {};

var stateData = fs.readFileSync('data/admin1CodesASCII.txt', 'utf-8');
var statePerLine = stateData.split('\n');
statePerLine.forEach(function(line) {
    row = line.split('\t');
    stateName[row[0]] = row[1];
});

var cityData = fs.readFileSync('data/cities_canada-usa.tsv', 'utf-8');
var cityPerLine = cityData.split('\n');
var headers = cityPerLine.shift().split('\t');

cityPerLine.forEach(function(line){
    if (line === '') {return;}
    row = line.split('\t');
    jsonObj = {};
    headers.forEach(function(key, i) {
        if (keepers.indexOf(key) !== -1) {
            jsonObj[key] = row[i];
        }
    });
    compositeKey = jsonObj['country'] + "." + jsonObj['admin1']
    jsonObj['fullname'] = jsonObj['name'] + "," + stateName[compositeKey] + "," + jsonObj['country'];
    cityName = S(row[1]).latinise().toString().toLowerCase();
    addOrAppend(cities, cityName, jsonObj);
    for (var i=1; i<cityName.length; i++) {
        partialCities.addOrAppend(cityName.substring(0,i), jsonObj);
    }
});

//req handling
//
module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    queryParams = url.parse(req.url, true).query;
    var matches = [];
    if (queryParams.q) {
        cityQ = S(queryParams.q).latinise().toString().toLowerCase();
        exactMatches(cityQ, matches);
        partialMatches(cityQ, matches)
    }
    if (queryParams.latitude && queryParams.longitude) {
        updateScoreForDistance(queryParams.latitude, queryParams.longitude, matches);
    }
    matches.sort(function(a, b) {return a.score > b.score ? -1 : 1})
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end(JSON.stringify({
        suggestions: matches
    }));
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '0.0.0.0');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
