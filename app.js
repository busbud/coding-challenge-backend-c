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
      // 1: {
      //   short: "AB",
      //   long: "Alberta"
      // },
      // 2: {
      //   short: "BC",
      //   long: "British Columbia"
      // },
      // 3: {
      //   short: "MB",
      //   long: "Manitoba"
      // },
      // 4: {
      //   short: "NB",
      //   long: "New Brunswick"
      // },
      // 5: {
      //   short: "NF",
      //   long: "Newfoundland & Labrador"
      // },
      // 7: {
      //   short: "NS",
      //   long: "Nova Scotia"
      // },
      // 8: {
      //   short: "ON",
      //   long: "Ontario"
      // },
      // 9: {
      //   short: "PE",
      //   long: "Prince Edward Island"
      // },
      // 10: {
      //   short: "QC",
      //   long: "Quebec"
      // },
      // 11: {
      //   short: "SK",
      //   long: "Saskatchewan"
      // },
      // 12: {
      //   short: "YT",
      //   long: "Yukon"
      // },
      // 13: {
      //   short: "NT",
      //   long: "Northwest Territories"
      // },
      // 14: {
      //   short: "NU",
      //   long: "Nunavut"
      // }
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
          { name: cityRegex},
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

  getSuggestions(req);
});

var server = app.listen(3000, function() {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Server running at http://%s:%s', host, port);
});

module.exports = app;
