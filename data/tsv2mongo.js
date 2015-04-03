var tsv = require('node-tsv-json');
var mongo = require('mongojs');
var async = require('async');
var helpers = require('../functions/helpers');

var colc = mongo('127.0.0.1:27017/busbud').collection('suggestions');

var provinces = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '13': 'NT',
  '14': 'NU',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT'
};


// Converts data from TSV to JSON
var convertData = function(cb) {
  console.log('Converting Data');
  tsv({
    input: './cities_canada-usa.tsv',
    output: null,
    parseRows: true

  }, function(err, result) {
    if (err) {
      console.log(err);
      process.exit(1);
    } else {
      cb(null, result);
    }
  });
};


// Parses through new JSON data, only takes the variables we need to reduce space and stores them in Mongo.
var toMongo = function(result, cb) {
  console.log('Inserting TSV to Mongo');
  async.each(result, function(e, acb) {
    var lngLat, name, state;

    if (e[0] !== 'id') { // Ignore first entry
      if (e[8] === 'CA') { // Canada or US?
        state = provinces[e[10]];
      } else {
        state = e[10];
      }

      name = e[1] + ' ' + state + ' ' + e[8];
      name = helpers.removeAccents(name);
      lngLat = {
        type: 'Point',
        coordinates: [parseFloat(e[5]), parseFloat(e[4])]
      };

      // Insert in Mongo
      colc.insert({
        phrase: name,
        city: e[1],
        lngLat: lngLat,
        country: e[8],
        state: state
      }, function() {
        acb(null);
      });

    } else {
      acb(null);
    }

  }, function(err) {
    if (err) {
      console.log(err);
    }
      cb(null);
  });
};

// Quick function to create indexs in Mongo.
var createIndexs = function(cb) {
  console.log('Creating Indexs');

  colc.createIndex({'phrase': 1}, {}, function(err) {
    if (err) {
      console.log(err);
    }

    colc.createIndex({'lngLat': '2dsphere'}, {}, function(err) {
      if (err) {
        console.log(err);
      }
      cb(null);

    });
  });
};

// Start her up.
async.waterfall([
  convertData,
  toMongo,
  createIndexs

  ], function() {
  console.log('Done');
  process.exit(0);
});
