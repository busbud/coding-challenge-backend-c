const express = require('express');
const search = require('./src/search');

const app = express();

const port = process.env.PORT || 2345;
const maxSearchResults = 10;

app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
  next();
});

app.get('/suggestions', (req, res) => {

  const query = req.query.q;
  const long = req.query.longitude;
  const lat = req.query.latitude;

  if (query === undefined || query === '') {
    res.writeHead(404, {
      'Content-Type': 'application/json',
    });

    res.end(JSON.stringify({
      suggestions: [],
    }));
  } else {
    const results = search.getElements(query, maxSearchResults, lat, long);

    results.then((result) => {
      if (result.length === 0) {
        res.statusCode = 404;
        res.setHeader('Content-Type', 'application/json');
      }

      res.json({
        suggestions: result,
      });
    }, (err) => {
      res.json(err);
    });
  }
});

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

module.exports = app;
