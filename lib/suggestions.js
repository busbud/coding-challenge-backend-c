var pg = require('pg');
var levenshtein = require('./levenshtein');
var haversine = require('./haversine');

// levenshtein returns value from 0 to max_length, where 0 is the best.
// We want to have a score that is 0 to 1 where 1 is the best, so we normalise levenshtein
function normalised_levenshtein(row, query) {
  max_ascii_len = Math.max(query.length, row.asciiname.length);
  max_name_len = Math.max(query.length, row.name.length);
  return Math.max(
    (max_ascii_len - levenshtein(query, row.asciiname)) / max_ascii_len, 
    (max_name_len - levenshtein(query, row.name))     / max_name_len
  );
}

function format_name(row) {
  var CA_ADMIN1_CODES = {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '05': 'NL',
    '07': 'NS',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'MB',
    '12': 'YT',
    '13': 'NT',
    '14': 'NU'
  }

  var country;
  if (row.country_code === "CA") {
    row.admin1_code = CA_ADMIN1_CODES[row.admin1_code];
    country = "Canada";
  } else {
    country = "USA";
  }
  return row.asciiname + ", " + row.admin1_code + ", " + country;
}

function format_row(row, query, latitude, longitude) {
  if (!latitude || !longitude) {
    return {
      name: format_name(row),
      latitude: row.latitude,
      longitude: row.longitude,
      score: normalised_levenshtein(row, query)
    };
  } else {
    // Put more weight on distance from city
    var score = (
      normalised_levenshtein(row, query) * 0.2 + 
      haversine(row.latitude, latitude, row.longitude, longitude) * 0.8
    )
    return {
      name: format_name(row),
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