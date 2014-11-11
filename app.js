var _ = require('lodash'),
    express = require('express'),
    app = express(),
    mongo = require('mongodb'),
    monk = require('monk'),
    db = monk('localhost:27017/cities'),
    cities = db.get('cities'),
    ID_TO_PROVINCE = {
      1: "AB",
      2: "BC",
      3: "MB",
      4: "NB",
      5: "NF",
      7: "NS",
      8: "ON",
      9: "PE",
      10: "QC",
      11: "SK",
      12: "YT",
      13: "NT",
      14: "NU"
    };

app.get('/suggestions', function(req, res) {
  function getSuggestions() {
    // if a querystring is submitted, query mongo
    if (req.query.q) {
      var cityRegex = new RegExp(req.query.q, 'i');
      var response = cities.find({
      // look for the cities which name's or alt_name's begin with the
      // querystring
        $or: [
          { ascii: cityRegex },
          { alt_name: cityRegex }
        ]},
        function(err, docs) {
          // if we have results, show them
          if (docs.length > 0) {
            res.status(200);
            res.json({ suggestions: formatSuggestions(docs) });
          // if we have no results, 404 and empty array
        } else if (docs.length === 0) {
          res.status(404);
          res.json({ suggestions: [] });
          }
        });
    } else {
      // if no querystring received, let the user know
      res.send(JSON.stringify({
        error: 'query parameter missing'
      }));
    }
    // mongo down
  }

  function formatSuggestions(suggestions) {
    var formattedSuggestions = [];
    _.each(suggestions, function(suggestion) {
      if (suggestion.country == 'CA') {
        // Translate the province ID to its short name
        suggestion.admin1 = ID_TO_PROVINCE[suggestion.admin1];
      }

      // Translate the country name
      if (suggestion.country == 'CA') suggestion.country = 'Canada';
      if (suggestion.country == 'US') suggestion.country = 'USA';

      formattedSuggestions.push({
        name: suggestion.ascii + ', ' + suggestion.admin1 + ', ' + suggestion.country,
        latitude: suggestion.lat,
        longitude: suggestion.long
      });
    });

    return formattedSuggestions;
  }

  function scoreSuggestions(suggestions, querystring, coordinates) {
    // 3 criterion:
    // 1. population
    // 2. exact match
    // 3. distance
    // score = average

    // A city with a population above this will score 100% for the population
    // criteria
    var criterionWeight = {
      exactitude: 0.7,
      population: 0.3
    },
        POPULATION_THRESHOLD = 300000, // That's 300,000
        score = {};

    _.each(suggestions, function(suggestion) {
    // score based on the querystring's exactitude
    // i.e. number of chars in querystring vs number of chars in ascii name
      score.exactitude = querystring.length / suggestion.ascii.length;
      score.population = Math.min(suggestion.population / POPULATION_THRESHOLD, 1);
      suggestion.score = score.exactitude * criterionWeigth.exactitude + score.population * criterionWeight.population;
    });
  }

  getSuggestions(req);
});

var server = app.listen(3000, function() {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Server running at http://%s:%s', host, port);
});

module.exports = app;
