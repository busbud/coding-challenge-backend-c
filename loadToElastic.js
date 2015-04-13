/* jshint node: true */
'use strict';

// Naitve requires
var fs = require('fs');

// Thrid party requires
var async = require('async');
var ProgressBar = require('progress');
var elasticsearch = require('elasticsearch');

// var host = process.env.ESHOST || 'localhost';

// var host = "192.241.145.84";
// var host = "localhost";
var host = "6a532498715436861371264a1ad33d04-us-east-1.foundcluster.com";

var index = 'north-america';
var type = 'city';

var client = new elasticsearch.Client({
  host: host + ':9200',
  // log: 'trace'
});

// Import data from file
var data = fs.readFileSync(__dirname + '/data/cities_canada-usa.tsv', 'utf8');

// Proccess TSV for converstion to JSON
data = data.split('\n');
var max = data.length - 1;

// Get TSV keys
var schema = data[0].split('\t');
data.shift();

// Initialize ProgressBar
var bar = new ProgressBar('Scraping [:bar] :percent :etas :elapseds :current/' + max, {
  complete: '=',
  incomplete: ' ',
  // width: 50,
  total: max
});

function tsvToJson(data) {
  var tmp = {};
  data.forEach(function(v, i, a) {
    tmp[schema[i]] = v;
  });

  return tmp;
}

/*    ** ELASTIC HELPER FUNCTIONS **    */

// Create the index needed for the file
function create() {
  console.log('Creating...');
  client.indices.create({
    index: index,
  }, map);
}

// Map the values that will be searched and their types
function map() {
  console.log('Mapping...');
  client.indices.putMapping({
    index: index,
    type: type,
    body: {
      city: {
        properties: {
          name: {
            type: "string"
          },
          location: {
            type: "geo_point",
            lat_lon: true
          }
        }
      }
    }
  }, insert);
}

// Insert elements into ES
function insert() {
  console.log('Inserting...');
  async.each(data, function(v, cb) {
    v = tsvToJson(v.split('\t'));

    if (v.lat && v.long) {
      var body = v;

      // Create ES geo-search object
      body.location = {
        lat: +v.lat,
        lon: +v.long
      };

      // Rename GEONAMES coordinates to standard
      body.latitude = body.lat;
      body.longitude = body.long;


      // Write item to database
      client.create({
        index: index,
        type: type,
        body: body
      }, function(e, r) {
        if (e) throw e;
        bar.tick(); // Tick through progressbar
        cb();
      });
    } else {
      bar.tick(); // Tick through progressbar
      cb();
    }
  }, done);
}

// Simple test case that returns JSON
function test() {
  console.log('Testing...');
  client.search({
    index: index,
    type: type,
    body: {
      sort: [{
        _geo_distance: {
          location: {
            lat: 43.70011,
            lon: -79.4163
          },
          order: "asc",
          unit: "km"
        }
      }],
      query: {
        match: {
          name: {
            query: "londo",
            fuzziness: "AUTO"
          }
        }
      }
    }
  }, function(e, r) {
    if (e) throw e;
    console.log(JSON.stringify(r, null, 4));
  });
}


// Start the application
function main() {
  client.indices.delete({
    index: index
  }, create);
}

// Callback to be ran when inserting is finished
function done() {
  console.log('Done!');
  process.exit(0);
}

return main();
// return test();
