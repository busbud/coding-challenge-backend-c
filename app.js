var express = require('express'),
    app = express(),
    port = process.env.PORT || 2345,
    _ = require('lodash'),
    haversine = require('haversine'),
    mongo = require('mongodb'),
    monk = require('monk'),
    // This is for Heroku
    db = monk((process.env.MONGOLAB_URI || 'localhost:27017') + '/cities'),
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

/*
Handle HTTP GET requests to the /suggestions endpoint
*/
app.get('/suggestions', function(req, res) {
  getSuggestions(req);

  /*
  This function queries mongoDB to try and find a suggestion. If a suggestion is
  found, it is then passed to be formatted. If none are found, it returns [] along
  with a 404
  */
  function getSuggestions(req) {
    // If a query string was submitted, hit mongo
    if (req.query.q) {
      var cityRegex = new RegExp(req.query.q, 'i');
      var limit = req.query.limit || 8;
      var response = cities.find(
      /*
      Look for the cities which ascii name's or alt_name's begin with the
      queryString. Searching alt_name allows to use airport codes, nicknames
      like 'NYC' etc. Searching ascii avoids dealing with diacritics.
      */
        {
          $or: [
            { ascii: cityRegex },
            { alt_name: cityRegex }
          ]
        },
        {
          limit: limit
        },
        function callback(err, docs) {
          // If we have results, format them
          if (docs.length > 0) {
            formatSuggestions(docs);
          // If we have no results, 404 and empty array
        } else if (docs.length === 0) {
          sendResponse([], '404');
          }
        });
    } else {
      // If no queryString received, let the user know
      sendResponse({error: 'query parameter missing'}, '400');
    }
  }

  /*
  This function formats a mongoDB result to the required JSON object. It is called
  only if mongoDB returned any results.
  */
  function formatSuggestions(suggestions) {
    var formattedSuggestions = [];
    _.each(suggestions, function(suggestion) {
      /*
      Translate the province ID to its short name if we are dealing with a
      Canadian city
      */
      if (suggestion.country == 'CA') {
        suggestion.admin1 = ID_TO_PROVINCE[suggestion.admin1];
      }

      // Translate the country name
      if (suggestion.country == 'CA') suggestion.country = 'Canada';
      if (suggestion.country == 'US') suggestion.country = 'USA';

      // Score the suggestion
      scoreSuggestions(suggestion, req.query);

      // Build the response object
      formattedSuggestions.push({
        name: suggestion.ascii + ', ' + suggestion.admin1 + ', ' + suggestion.country,
        latitude: suggestion.lat,
        longitude: suggestion.long,
        score: suggestion.score
      });
    });

    // Order suggestions by score
    orderSuggestions(formattedSuggestions, 'score');
  }

  function scoreSuggestions(suggestion, query) {
    /*
    Results are scored according to the following criterions:
    - Distance from the supplied lat/long (if supplied)
    - City's population
    - Difference between the length of the supplied query string and the actual
      city's name
    Each of the results carry a different importance (weight) and a final score is
    computed. The final score is between 0 and 1.
    */

    /*
    A city with a population above POPULATION_THRESHOLD will score full points on
    the population part of the total score.
    */
    var POPULATION_THRESHOLD = 1000000, // That's 1,000,000
        score = {},
        criterionWeight;

    /*
    Adjust the weight of the different criterions depending on whether we received
    a lat/long with the query or not.
    Distance will only be scored if lat/long were received in the query.
    */
    if (query.latitude && query.longitude) {
      criterionWeight = {
        distance: 0.7,
        exactitude: 0.2,
        population: 0.1
      };
    } else {
      criterionWeight = {
        exactitude: 0.7,
        population: 0.3
      };
    }

    /*
    Scoring the distance from the supplied coordinates, if applicable
    */
    if (query.latitude && query.longitude) {
      var cityCoordinates = {
            latitude: suggestion.lat,
            longitude: suggestion.long
          },
          queryCoordinates = {
            latitude: query.latitude,
            longitude: query.longitude
          };

      /*
      The haversine is a way to calculate distances between two latitudes and
      longitudes, cf http://en.wikipedia.org/wiki/Haversine_formula.
      The lower the score, the further the city is from the supplied lat/long.
      */
      distance = haversine(cityCoordinates, queryCoordinates);
      if (distance <= 50) {
        score.distance = 1;
      } else if (distance >  50 && distance <=  150) {
        score.distance = 0.7;
      } else if (distance > 150 && distance <=  400) {
        score.distance = 0.5;
      } else if (distance > 400 && distance <= 1000) {
        score.distance = 0.3;
      } else {
        score.distance = 0;
      }
    }

    /*
    Scoring how much the city's name matches with the supplied query string.
    Because the mongoDB looks for a regex of the query string, we are certain that
    100% of the query string is in the name. The lengths of the query string and
    the actual name are compared and the closer the two lengths are, the higher
    the score for the exactitude criteria.
    */
    score.exactitude = query.q.length / suggestion.ascii.length;

    /*
    Scoring the  city's size by population. If the city's population is under the
    POPULATION_THRESHOLD, calculate a fraction of the full score.
    If the population is above the POPULATION_THRESHOLD, then the city gets a full
    point for that criteria.
    */
    score.population = Math.min(suggestion.population / POPULATION_THRESHOLD, 1);

    /*
    Now we calculate the total score which is a weighted average of the 2 or 3
    score components, depending on whether we scored distance.
    */
    if (query.latitude && query.longitude) {
      suggestion.score = score.distance * criterionWeight.distance + score.exactitude * criterionWeight.exactitude + score.population * criterionWeight.population;
    } else {
      suggestion.score = score.exactitude * criterionWeight.exactitude + score.population * criterionWeight.population;
    }

    // Round the score to a 4 decimals number, for a better readability.
    suggestion.score = suggestion.score.toFixed(4);

    return suggestion;
  }

  /*
  Reorders the suggestions by the `by` argument.
  */
  function orderSuggestions(suggestions, by) {
    orderedSuggestions = _.sortBy(suggestions, by).reverse();
    sendResponse(orderedSuggestions, '200');
  }

  /*
  Sends the response with a HTTP status code of `status`
  */
  function sendResponse(object, status) {
    res.status(status);
    res.json({ suggestions: object });
  }
});


/*
Boilerplate expressJS webserver code
*/
var server = app.listen(port, function() {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Suggestions API server running at http://%s:%s', host, port);
});

module.exports = app;
