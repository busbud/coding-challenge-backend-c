const express = require('express');
const app = express();
const search = require('./src/search');

var port = process.env.PORT || 2345;

app.get('/suggestions', function(req, res) {

  var query = req.query.q;
  var long = req.query.long;
  var lat = req.query.lat;

  if (query === undefined || query === "") {

    res.writeHead(404, {'Content-Type': 'application/json'});

    res.end(JSON.stringify({
      suggestions: []
    }));

  } else {

    var result = search.getElements(query, 10, lat, long);

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
