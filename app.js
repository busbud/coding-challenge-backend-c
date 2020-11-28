const http = require('http');
const express = require('express');
const accents = require('remove-accents');
const db = require('./src/db');
const distance = require('gps-distance');
const PORT = process.env.PORT || 2345;

const app = express();

const scoreCity = (searchCity, searchLat, searchLong, asciiName, lat, long) => {
  let score = 0;
  const weights = {
    doesNotStartWithWeight: 3,
    letterMatchWeight: 2,
    distanceWeight: 0.1
  }

  // Check if name starts search
  const re = new RegExp(`^${searchCity}`);

  if (!asciiName.match(re)) {
    score += 1 * weights.doesNotStartWithWeight;
  }

  // Score strength of string match
  const nameLengthDiff = asciiName.length - searchCity.length;
  weightTotal += weights.letterMatchWeight;
  score += nameLengthDiff * weights.letterMatchWeight;

  // Score gps distance
  if (searchLat && searchLong) {
    const path = [
      [ searchLat, searchLong],
      [lat, long]
    ];

    const gpsDistance = distance(path);

    weightTotal += weights.distanceWeight;
    score += gpsDistance * weights.distanceWeight;
  }

  return score;
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
    });

    return res.json({ suggestions });
  }
  catch (e) {
    return res.json({ e });
    // TODO: handle error
  }
});

const server = http.createServer(app);
server.listen(PORT, () => console.log('Server running at http://0.0.0.0:%d/suggestions', PORT));