const express = require('express');
const search = require('./src/search');

const app = express();

const port = process.env.PORT || 2345;
const maxSearchResults = 10;

app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

app.get('/suggestions', function(req, res) {

  var query = req.query.q;
  var long = req.query.longitude;
  var lat = req.query.latitude;

  if (query === undefined || query === "") {

    res.writeHead(404, {'Content-Type': 'application/json'});

    res.end(JSON.stringify({
      suggestions: []
    }));

  } else {

    var result = search.getElements(query, maxSearchResults, lat, long);

    result.then(function (result) {

      if (result.length === 0) {
        res.statusCode = 404;
        res.setHeader('Content-Type', 'application/json');
      }

      res.json({
        suggestions: result
      });

    }, function(err) {

      res.json(err);

    });

  }
  
});

app.listen(port, function () {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});


module.exports = app;