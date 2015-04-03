var mongo = require('mongojs');
var async = require('async');
var _ = require('lodash');
require('string_score');
var helpers = require('./helpers');

// var colc = mongo('127.0.0.1:27017/busbud').collection('suggestions');
var colc = mongo('read:read@ds059651.mongolab.com:59651/heroku_app35517737').collection('suggestions');

var defaultResultsLimit = 10;  // Set a default limit for how many results to return to client


// Builds the query by removing any accents in the search (as our database has no accents for the search field),
// Our query is using regex to match/compare to the beginning of each key in all documents in Mongo.
// Then checks if lat/lon was included, and if so add it to the query so it automaticially sorts by isNear.
var runQuery = function(initData, cb) {
  var getQuery = initData.getQuery;
  var name = helpers.removeAccents(getQuery.q);
  var re = new RegExp('^' + name, 'i');
  var lat = getQuery.latitude;
  var lon = getQuery.longitude;

  // Initial Query
  var query = {
    'phrase': {
      '$regex': re
    }
  };

  // LatLon?
  if (lat && lon) {
    query.lngLat = {
      '$near': {
        '$geometry': {
          'type': 'Point',
          'coordinates': [parseFloat(lon), parseFloat(lat)]
        }
      }
    };
  }

  // Query and callback
  colc.find(query).limit(5000, function(err, results) {
    if (err) {
      console.log(err);
    }
    cb(null, initData, results);
  });
};


// This builds the object that will be appending to the array that's going back to the client.
// Also tidding up name to include city, state, and country, and by doing this includes any cities that have accents.
// Then generate the initial score based on string compare using the string_score module.
var generateScore = function(initData, results, cb) {
  var getQuery = initData.getQuery;

  async.map(results, function(item, acb) {
    var score = item['phrase'].score(getQuery.q);  // Generate score using string compare

    var post = {  // What we'll be returning
      score: score,
      name: item.city + ', ' + item.state + ', ' + item.country,  // Create a new name key
      latitude: item.lngLat.coordinates[1],
      longitude: item.lngLat.coordinates[0]
    };
    acb(null, post);

  }, function(err, mapResults) {
    // Now we check if we search by lat/lon. If there's no lat/lon, the results get sent to the sorting function.
    // However, if lat/lon is available, we want the nearest results to come up first!
    // As Mongo returns the results nearest/farthest, we reverse the array so the nearest result is at the end,
    // Then using an array index, we multiply each entry by a small float to increase each entry based on distance (which is it's position in the array).
    // Afterwards, this gets sent to the sorting function, which results in the nearest location coming up first!

    if (getQuery.latitude && getQuery.longitude) {
      mapResults.reverse();
      async.each(_.range(0, mapResults.length), function(i, acb) {

        var score = mapResults[i].score - 0.1;
        var newScore = score + (i * 0.005);
        if (newScore > 1) {
          newScore = 1;
        }

        mapResults[i].score = newScore;
        acb(null);

      }, function(err) {
        if (err) {
          console.log(err);
        }
        cb(null, initData, mapResults);
      });
    } else {
      cb(null, initData, mapResults);
    }
  });
};

// Sorts the results by score, highest to lowest
var sortResults = function(initData, results, cb) {
  results.sort(helpers.sortByScore);
  results = results.slice(0, initData.resultsLimit);
  cb(null, results);
};


// Executes the magic. This also includes a runtime as I wanted to see how fast the query was.
// Query also removes everything but letters, numbers, and spaces from the users request to avoid conflicts.
module.exports = function(req, res) {
  getQuery = req.query;

  var resultsLimit;
  if (!isNaN(getQuery.limit)) {
    resultsLimit = parseInt(getQuery.limit);
  } else {
    resultsLimit = defaultResultsLimit;
  }

  if (!getQuery.q) {  // Quickly check if q is defined. If it isn't, return early.
    res.json({
      error: 'Need to define q get parameter'
    });

  } else {
    var name = req.query.q.replace(/[^A-Za-z0-9\s ]/g, '').toLowerCase();

    // *insert Harry Potter spell here*
    async.waterfall([
      function(cb) {
        initData = {
          resultsLimit: resultsLimit,
          getQuery: getQuery
        };
        cb(null, initData);
      },
      runQuery,
      generateScore,
      sortResults
      ], function(err, results) {

      if (err) {
        console.log(err);
      }

      var post = {  // Build object were returning
        suggestions: results
      };

      if (results.length === 0) {  // Check if there's 0 results, if so set response code to 404
        res.status(404).json(post);
      } else {
        res.json(post);
      }
    });
  }
};
