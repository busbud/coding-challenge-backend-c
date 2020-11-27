const http = require('http');
const express = require('express');
const db = require('./src/db');
const PORT = process.env.PORT || 2345;

const app = express();

/**
 * GET /suggestions
 * TODO: Docs
 */
app.get('/suggestions', async (req, res) => {
  try {
    const {q, latitude=null, longitude=null} = req.query

    // Form and execute query
    const query = `SELECT * FROM cities_mv WHERE tsquery('${q}' || ':*') @@ to_tsvector(name);`
    const data = await db(query);

    // Build city response structure
    const suggestions = data.rows.map((row) => {
      const { ascii, country, admin1, lat, long } = row;
      let countryFull;

      if (country === "US") countryFull = "USA";
      else if (country == "CA") countryFull = "Canada"
      else countryFull = "";

      const suggestion = {
        name: `${ascii}, ${admin1}, ${countryFull}`,
        latitude: lat,
        longitude: long,
        score: 0
      };

      return suggestion;
    });

    return res.json({ suggestions });
  }
  catch (e) {
    // TODO: handle error
  }
});

const server = http.createServer(app);
server.listen(PORT, () => console.log('Server running at http://0.0.0.0:%d/suggestions', PORT));