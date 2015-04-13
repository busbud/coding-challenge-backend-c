/* jshint node: true */
'use strict';

var elasticsearch = require('elasticsearch');

var host = process.env.PORT || 'localhost';

var client = new elasticsearch.Client({
  host: host + ':9200',
  // log: 'trace'
});

var http = require('http');
var https = require('https');

var express = require('express'),
  app = express();

// var port = process.env.PORT || 2345;
var port = 80;

function search(q, cb) {
  var query = q.q || q.query || '';
  var lat = q.latitude || q.lat;
  var lon = q.longitude || q.lon;
  var sort = ["_score"];

  if (lat && lon) {
    sort.push({
      _geo_distance: {
        location: {
          lat: lat,
          lon: lon
        },
        order: "asc",
        unit: "km"
      }
    });
  }

  client.search({
    index: 'north-america',
    type: 'city',
    body: {
      track_scores: true,
      sort: sort,
      query: {
        match: {
          name: {
            query: query,
            fuzziness: "AUTO",
            prefix_length: 0
          }
        }
      }
    }
  }, cb);
}

function pretty(uson) {
  var pson = {};
  pson.suggestions = [];
  if (uson.hits) {
    // pson.maxscore = uson.hits.max_score;
    pson.count = uson.hits.total;
  }
  pson.time = uson.took;

  uson.hits.hits.forEach(function(v, i, a) {
    var tmp = {};
    var roof = Math.ceil(uson.hits.max_score);

    tmp.name = v._source.ascii;
    tmp.address = v._source.name;

    if (isNaN(+v._source.admin1)) {
      tmp.address += ", " + v._source.admin1;
    }

    tmp.address += ", " + v._source.country;
    tmp.score = v._score / roof;
    tmp.id = +v._source.id;
    tmp.latitude = +v._source.latitude;
    tmp.longitude = +v._source.longitude;

    pson.suggestions.push(tmp);
  });

  return pson;

}

app.get('/raw/suggestions', function(req, res) {
  search(req.query, function(e, r) {
    res.json(r);
  });
});

app.get('/suggestions', function(req, res) {
  search(req.query, function(e, r) {
    var json = pretty(r);

    if (json.suggestions.length === 0) {
      res.status(404);
    }

    res.json(json);
  });
});



console.log('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = app.listen(port);
