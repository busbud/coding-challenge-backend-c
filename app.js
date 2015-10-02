require('dotenv').load();

var express      = require('express'),
    app          = express(),
    pg           = require('pg');

function format_row(row, latitude, longitude) {
  if (!latitude || !longitude) {
    return {
      name: row.asciiname,
      latitude: row.latitude,
      longitude: row.longitude
    };
  } else {
    var score = 0;
    return {
      name: row.asciiname,
      latitude: row.latitude,
      longitude: row.longitude,
      score: score
    };
  }
}

function compare_suggestions(a, b) {
  if (typeof a.score !== 'undefined' && typeof b.score !== 'undefined' && a.score !== b.score) {
    return a.score - b.score;
  }
  // If we cannot sort by score, or no diff between score, we will sort by name;
  if (a.name < b.name) return -1;
  if (a.name > b.name) return 1;
  return 0;
}

app.get('/suggestions', function(req, res) {
  if (!req.query.q) {
    return res.status(404).type('json').send(JSON.stringify({suggestions:[]}));
  }
  var partQ = req.query.q + "%";
  pg.connect(process.env.DATABASE_URL, function(err, client, done) {
    client.query("SELECT asciiname, latitude, longitude, country_code, admin1_code FROM geoname WHERE name LIKE $1 OR asciiname LIKE $2;", 
      [partQ, partQ], 
      function(err, result) {
        if (err) {
          console.error(err);
          return res.status(500).send("Database error");
        }
        done();
        if (!result.rows.length) {
          return res.status(404).type('json').send(JSON.stringify({suggestions:[]}));
        }

        var suggestions = result.rows.map(function(row) {
          console.log(format_row(row, req.query.latitude, req.query.longitude));
          return format_row(row, req.query.latitude, req.query.longitude);
        });

        suggestions = suggestions.sort(compare_suggestions);

        return res.status(200).type('json').send(JSON.stringify({suggestions: suggestions}));
      }
    );
  });
});

var server = app.listen(process.env.PORT || 2345, function() {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Suggestions api listening at http://%s:%s/suggestions', host, port);
});

module.exports = app;