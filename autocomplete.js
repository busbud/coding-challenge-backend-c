var url = require('url');
var redis = require('redis')
var converter = require('./utils/tsvConverter');
var strings = require('./utils/strings');

const AUTOCOMPLETE_DATA  = 'cityautocomplete:data';
const AUTOCOMPLETE_INDEX = 'cityautocomplete:index';

var redisURL = url.parse(process.env.REDISCLOUD_URL || 'redis://127.0.0.1:6379');
r = redis.createClient(redisURL.port, redisURL.hostname, {no_ready_check: true});
if(redisURL.auth) {
  r.auth(redisURL.auth.split(":")[1]);  
}

exports.populate = function() {
  converter.toJson('data/cities_canada-usa.tsv', function(json) {
    //console.log(json)
    json.forEach(function(city) {
      addCity(city)
    });
  });
}

exports.search = function(prefix, callback) {
  r.zrange(AUTOCOMPLETE_INDEX + ':' + prefix, 0, -1, function(err, ids) {
    r.hmget(AUTOCOMPLETE_DATA, ids, function(err, cities) {
      var citiesJson = []
      if(cities != undefined) {
        citiesJson = JSON.parse('[' + cities + ']')
      }
      callback(citiesJson);
    });
  });
}

exports.clear = function() {
  // This is not safe if you have more than just the autocomplete data in redis
  // Just a quick way to debug and benchmark
  r.flushdb();
}

var addCity = function(city) {
  var prefixes = strings.prefixesFor(city['name']);

  // Add every prefix to the ordered set with the city ID as the value
  prefixes.forEach(function(prefix) {
    r.zadd(AUTOCOMPLETE_INDEX + ':' + prefix, 0, city['id']);
  })

  // Add the city ID to the HSET with the JSON as value
  r.hset(AUTOCOMPLETE_DATA, city['id'], JSON.stringify(city));
}
