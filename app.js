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

    const suggestedCities = await database_pool.query(
      "SELECT concat(name, ', ', admin1, ', ', country) AS name, lat AS latitude, long AS longitude FROM geoname WHERE population > 5000 AND country IN ('US', 'CA') AND name iLIKE $1 AND (lat = $2 OR $2 IS NULL ) AND (long = $3 OR $3 IS NULL)",
      [q + "%", lat, long]
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
