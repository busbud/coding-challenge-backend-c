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
    let { q, latitude, longitude } = req.query;

    if (!q) {
      return res.status(404).send("Sorry, we cannot find that!");
    }

    const suggestedCities = await database_pool.query(
      // "SELECT name, alt_name, lat, long FROM geoname WHERE population > 5000 AND country IN ('US', 'CA')"
      "SELECT name, alt_name, lat, long FROM geoname WHERE population > 5000 AND country IN ('US', 'CA') AND name iLIKE $1",
      [q + "%"]
    );

    res.json(suggestedCities);
  } catch (err) {
    next(err);
  }
});

app.listen(port, () => {
  console.log("Server running at http://127.0.0.1:%d", port);
});
