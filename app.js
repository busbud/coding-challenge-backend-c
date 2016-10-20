var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');
var express = require('express');
var querystring = require('querystring');
var searchController = require('./lib/search');

//Loading database on server start.
var DataParser = require('./lib/dataParser');
var data = new DataParser({});

var app = express();
app.use(express.static(__dirname + '/public'));
app.set('views', __dirname + '/views');
app.engine('html', require('ejs').renderFile);

// Home
app.get('/', function(req, res) {
  res.render('index.html');
});

app.get('/suggestions', function(req, res) {
  var parsedUrl = url.parse(req.url);

  if (req.url.indexOf('/suggestions') === 0) {
    var options = querystring.parse(parsedUrl.query);
    var errors = '';

    var queryString = options.q || '';
    if (!options.q) {
      errors += 'q parameter is required and must be a string.';
    }
    if (options.latitude != undefined && isNaN(parseFloat(options.latitude))) {
      errors += 'latitude param must be numeric.';
    }
    if (options.longitude != undefined && isNaN(parseFloat(options.longitude))) {
      errors += 'longitude param must be numeric.'
    }
    if (options.limit != undefined && isNaN(parseInt(options.limit))) {
      errors += 'limit param must be numeric.'
    }

    if (errors) {
      res.status(400).json({
        errors: errors,
        suggestions: []
      });
      return;
    }

    searchController.getSuggestions(data.cities, options, function(err, suggestions) {
      if(err) {
        res.writeHead(500);
        return res.end();
      }
      if(suggestions.length) {
        res.writeHead(200);
      } else {
        res.writeHead(404);
      }
      res.end(JSON.stringify({
        suggestions: suggestions
      }));
    });
  } else {
    res.end();
  }
});

// Run app
app.listen(port);
console.log('Server running at http://127.0.0.1:%d/suggestions', port);

module.exports = app;
