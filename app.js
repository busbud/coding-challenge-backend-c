const express = require("express");
const app = express();
const database_pool = require("./db/index");

const port = process.env.PORT || 2345;

app.get("/", async (req, res, next) => {
  try {
    const allCities = await database_pool.query("SELECT *  FROM geoname");
    res.json(allCities);
  } catch (err) {
    next(err);
  }
});

app.get("/suggestions", async (req, res, next) => {
  try {
    let { q, lat, long } = req.query;
    let latMax = parseInt(lat) + 3;
    let latMin = parseInt(lat) - 3;
    let longMax = parseInt(long) + 3;
    let longMin = parseInt(long) - 3;
    let score = 0;

    if (!q) {
      return res.status(404).send("Please enter a valid search term");
    }

    const suggestedCities = await database_pool.query(
      "SELECT concat(name, ', ', admin1, ', ', country) AS name, lat AS latitude, long AS longitude, $8 AS score FROM geoname WHERE population > 5000 AND country IN ('US', 'CA') AND (name iLIKE $1 OR ascii iLIKE $1 OR ascii iLIKE $1) AND (lat BETWEEN $2 AND $3 OR lat = $4 IS NULL) AND (long BETWEEN $5 AND $6 OR long = $7 IS NULL)",
      [q + "%", latMin, latMax, lat, longMin, longMax, long, score]
    );

    if (suggestedCities.rowCount === 0) {
      return res
        .status(404)
        .send(
          "No matches available.",
          res.json({ suggestions: suggestedCities.rows })
        );
    } else {
      res.json({ suggestions: suggestedCities.rows });
    }
  } catch (err) {
    next(err);
  }
});

app.listen(port, () => {
  console.log("Server running at http://127.0.0.1:%d", port);
});

module.exports = app;
