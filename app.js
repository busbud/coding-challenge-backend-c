const http = require('http');
const express = require('express');
const accents = require('remove-accents');
const db = require('./src/db');
const distance = require('gps-distance');
const PORT = process.env.PORT || 2345;

const app = express();

const scoreCity = (searchCity, searchLat, searchLong, asciiName, lat, long) => {
  let score = 0;
  // TODO: Fine tune wieghts
  const weights = {
    doesNotStartWithWeight: 5,
    letterMatchWeight: 7,
    distanceWeight: 0.1
  }

  // Check if name starts search
  const re = new RegExp(`^${searchCity}`);

  if (!asciiName.match(re)) {
    score += 1 * weights.doesNotStartWithWeight;
  }

  // Score strength of string match
  const nameLengthDiff = asciiName.length - searchCity.length;
  score += nameLengthDiff * weights.letterMatchWeight;

  // Score gps distance
  if (searchLat && searchLong) {
    const path = [
      [ searchLat, searchLong],
      [lat, long]
    ];

    const gpsDistance = distance(path);

    score += gpsDistance * weights.distanceWeight;
  }

  return score;
}

const normalizeAndSort = (suggestions) => {
  // Find max value for normalization
  const scores = suggestions.reduce((scores, suggestion) => {
    scores.push(suggestion.score);
    return scores;
  }, []);
  const ratio = Math.max.apply(Math, scores) / 100;

  // Normalize and invert; lowest score was closest
  const normalized = suggestions.map((suggestion) => {
    const normal = 1 - (Math.round(suggestion.score / ratio) / 100);

    suggestion.score = Math.round((normal + Number.EPSILON) * 100) / 100

    return suggestion
  });

  // Sort
  const sorted = normalized.sort((a, b) => {
    if (a.score < b.score) return 1;
    if (a.score > b.score) return -1;
    return 0;
  });

  console.log(sorted);
  return sorted;
}

/**
 * GET /suggestions
 * TODO: Docs
 */
app.get('/suggestions', async (req, res) => {
  try {
    const {q, latitude=null, longitude=null} = req.query

    // Strip diacritics
    const city = accents.remove(q).toLowerCase();

    // Form and execute query
    const query = `SELECT * FROM cities_mv WHERE tsquery('${city}' || ':*') @@ to_tsvector(ascii);`
    const data = await db(query);

    // Build city response structure
    const suggestions = data.rows.map((row) => {
      const { ascii, country, admin1, lat, long } = row;
      let countryFull;

      if (country === "US") countryFull = "USA";
      else if (country == "CA") countryFull = "Canada"
      else countryFull = "";

      // Calculate score
      const score = scoreCity(city, latitude, longitude, ascii, lat, long);

      const suggestion = {
        name: `${ascii}, ${admin1}, ${countryFull}`,
        latitude: lat,
        longitude: long,
        score: score
      };

      return suggestion;
    })

    const normalizedSuggestions = normalizeAndSort(suggestions);

    return res.json({ suggestions: normalizedSuggestions });
  }
  catch (e) {
    console.error(e);
    return res.json({ e });
    // TODO: handle error
  }
});

const server = http.createServer(app);
server.listen(PORT, () => console.log('Server running at http://0.0.0.0:%d/suggestions', PORT));