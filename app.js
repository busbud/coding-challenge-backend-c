const express = require("express");
const app = express();
const database_pool = require("./db/index");

const port = process.env.PORT || 2345;

//GET all rows from TABLE geoname
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

    console.log(longMax, "\n", longMin);

    const suggestedCities = await database_pool.query(
      "SELECT concat(name, ', ', admin1, ', ', country) AS name, lat AS latitude, long AS longitude FROM geoname WHERE population > 5000 AND country IN ('US', 'CA') AND name iLIKE $1 AND (lat BETWEEN $2 AND $3 OR lat = $4 IS NULL) AND (long BETWEEN $5 AND $6 OR long = $7 IS NULL)",
      [q + "%", latMin, latMax, lat, longMin, longMax, long]
    );

    // if (!q || suggestedCities.rows.length === 0) {
    if (!q) {
      return res.status(404).send("Sorry, we cannot find that!");
    }
    res.json(suggestedCities.rows);
  } catch (err) {
    next(err);
  }
});

app.listen(port, () => {
  console.log("Server running at http://127.0.0.1:%d", port);
});

module.exports = app;
