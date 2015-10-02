var pg = require('pg');
var levenshtein = require('./levenshtein');

function normalised_levenshtein(row, query) {
  max_ascii_len = Math.max(query.length, row.asciiname.length);
  max_name_len = Math.max(query.length, row.name.length);
  return Math.max(
    (max_ascii_len - levenshtein(query, row.asciiname)) / max_ascii_len, 
    (max_name_len - levenshtein(query, row.name))     / max_name_len
  );
}

function format_row(row, query, latitude, longitude) {
  if (!latitude || !longitude) {
    return {
      name: row.asciiname,
      latitude: row.latitude,
      longitude: row.longitude,
      score: normalised_levenshtein(row, query)
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
    return b.score - a.score;
  }
  // If we cannot sort by score, or no diff between score, we will sort by name;
  if (a.name < b.name) return -1;
  if (a.name > b.name) return 1;
  return 0;
}

module.exports = function(app) {
  app.get('/suggestions', function(req, res) {
    if (!req.query.q) {
      return res.status(404).type('json').send(JSON.stringify({suggestions:[]}));
    }
    var partQ = req.query.q + "%";
    pg.connect(process.env.DATABASE_URL, function(err, client, done) {
      client.query("SELECT asciiname, name, latitude, longitude, country_code, admin1_code FROM geoname WHERE name LIKE $1 OR asciiname LIKE $2;", 
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
            return format_row(row, req.query.q, req.query.latitude, req.query.longitude);
          });

          suggestions = suggestions.sort(compare_suggestions);

          return res.status(200).type('json').send(JSON.stringify({suggestions: suggestions}));
        }
      );
    });
  });
};