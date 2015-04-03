// Import
var express = require("express");
var path = require("path");
var colors = require("colors");
var queryMongo = require('./functions/queryMongo');

// Setup Express
var app = express();
var server = require("http").Server(app);
var port = process.env.PORT || 5051;
app.use(express["static"](path.join(__dirname, 'public')));

// Routers
var router = express.Router();

router.get('/', function(req, res) {
  res.sendFile('./public/index.html');
});

router.get('/suggestions', queryMongo);

// Last Express Setup
app.use("/", router);

app.use(function(req, res, next) {
  res.status(404).send('404');
});

app.use(function(err, req, res, next) {
  console.error(err.stack);
  res.status(500).send('Error: ' + err.stack);
});

// Boot her up.
module.exports = server.listen(port);
console.log(("Express Started | Magic happens on port " + port).bold.green);
