
/********************
 * Import Modules
 *******************/

var http = require("http"),
  Express = require("express"),
  NodeBusy = require("toobusy"),
  BodyParser = require("body-parser");

var _ = require("lodash");


var Busbud = require(__dirname + "/app/busbud.js");




/********************
 * Node Server Set up
 *******************/

var app = Express(),
  port = process.env.PORT || 2345;

var busbud = new Busbud();


// First Middleware
app.use(function (req, res, next) {
  // Set up trie
  req.system = {};
  req.system.busbud = busbud;

  // Check if too busy
  if (NodeBusy()) {
    return res.send(503, "Server is unavailable.");
  } else {
    return next();
  }
});

// Parse body, this module only has urlencoded and json
// Safe body parser
app.use(BodyParser());



// Route for Suggestions
app.post("/suggestions", function (req, res, next) {

});





/********************
 * Module Exports
 *******************/


module.exports = app.listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);




/********************
 * Node Server Set up
 *******************/


/*
module.exports = http.createServer(function (req, res) {
    res.writeHead(404, {'Content-Type': 'text/plain'});

    if (req.url.indexOf('/suggestions') === 0) {
      res.end(JSON.stringify({
        suggestions: []
      }));
    } else {
      res.end();
    }
  }).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
*/



