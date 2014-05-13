
/********************
 * Import Modules
 *******************/

var http = require("http"),
  Express = require("express"),
  NodeBusy = require("toobusy"),
  BodyParser = require("body-parser");

var _ = require("lodash");


var Busbud = require(__dirname + "/busbud/busbud.js");

var Ranking = require(__dirname + "/busbud/algorithms/ranking.js");




/********************
 * Node Server Set up
 *******************/

// Busbud interface
var busbud = new Busbud(__dirname + "/data/cities_canada-usa.tsv", function (err, data) {
  // Check if error
  if (err) {
    throw new Error(err);
  }
});


// Express server
var app = Express();


// Check what environment we are
// Node too busy messes up with mocha unit testing since it returns 503's
if (process.env.NODE_ENV === "production") {
  app.use(function (req, res, next) {
    // Check if too busy
    if (NodeBusy()) {
      return res.send(503, "Server is unavailable.");
    } else {
      return next();
    }
  });
}


// Parse body, this module only has urlencoded and json
// Safe body parser
app.use(BodyParser());



// Route for Suggestions
app.get("/suggestions", function (req, res) {
  // Get query information
  if (req.query) {
    // Get query string
    var _string = req.query.q;

    // Check if valid
    if (_.isString(_string)) {
      // Initialize tokens and output
      var _tokens = busbud.analyzer.analyze(_string),
        _trieOutput = [];

      // Initialize latitude, longitude
      var _latitude = req.query.latitude,
        _longitude = req.query.longitude;

      // Get output for each token
      for (var i = 0; i < _tokens.length; i++) {
        _trieOutput.push(busbud.trie.traverse(_tokens[i], _latitude, _longitude));
      }

      // Compute result
      var _results = Ranking.ReduceRanking(_trieOutput, 10);
      if (_results.length > 0) {
        return res.json(200, { suggestions: _results });
      } else{
        return res.json(404, { suggestions: [] });
      }
    }
  }

  return res.json({});
});


var _port = process.env.PORT || 2345;
app.listen(_port);
console.log("Server started! Port: " + _port + ".");



/********************
 * Module Exports
 *******************/

module.exports = app;
