const http = require('http');
const express = require('express');
const accents = require('remove-accents');
const db = require('./src/db');
const distance = require('gps-distance');
const PORT = process.env.PORT || 3000;

const app = express();

/**
 * Simple scoring function to see how close a city from the database is to the search
 * terms. The higher the score, the "farther away" a city is from the search terms and
 * thus the lower the end percentage match will be.
 *
 * @param {String} searchCity: Search query from api call
 * @param {Number} searchLat: Optional latitude from api call
 * @param {Number} searchLong: Optional Longitude from api call
 * @param {String} asciiName: Full name of city for comparison to search
 * @param {Number} lat: Latitude of city for comparison to search
 * @param {Number} long: Longitude of city for comparison to search
 *
 * @returns {Number} score: The final calculated score
 */
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

/**
 * Normalize score values to fall between 0 and 1, then sort the normalized values to
 * order suggestions from best to worst.
 *
 * @param {Array} suggestions An array of unsorted city suggestions matching search terms
 *
 * @returns {Array} sorted: A sorted array of city suggestions in decending order by score
 */
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
 * Creates and returns an ordered list of potential city suggestions matching a partial or
 * full search query q. Provides optional support for user supplied GPS coordinates to fine
 * tune search results.
 *
 * @param {String} q: Partial of full city name used in primary search query
 * @param {Number} latitude: [Optional] User supplied latitude for positional relevance in scoring
 * @param {Number} longitude: [Optional] User supplied longitude for positional relevance in scoring
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

    // No results found - Return 404
    if (suggestions.length == 0) {
      return res.status(404).json({ suggestions });
    }

    // Sort and format final response
    const normalizedSuggestions = normalizeAndSort(suggestions);

    return res.json({ suggestions: normalizedSuggestions });
  }
  catch (error) {
    // Unhandled exception - Return 500
    return res.status(500).json({ error });
  }
});

const server = http.createServer(app);
server.listen(PORT, () => console.log('Server running at http://0.0.0.0:%d/suggestions', PORT));