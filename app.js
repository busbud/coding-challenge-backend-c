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
    let { q, latitude, longitude } = req.query;
    let coordinateMax = (coordinate) => parseInt(coordinate) + 2;
    let coordinateMin = (coordinate) => parseInt(coordinate) - 2;
    let score = 0;

    if (!q) {
      return res.status(404).send("Please enter a valid search term");
    }

    const suggestedCities = await database_pool.query(
      "SELECT concat(name, ', ', admin1, ', ', country) AS name, lat AS latitude, long AS longitude, $2 AS score FROM geoname WHERE population > 5000 AND country IN ('US', 'CA') AND (name iLIKE $1 OR ascii iLIKE $1 OR ascii iLIKE $1)",
      [q + "%", score]
    );
    if (suggestedCities.rowCount === 0) {
      return res
        .status(404)
        .send(
          "No matches available.",
          res.json({ suggestions: suggestedCities.rows })
        );
    } else {
      suggestedCities.rows.forEach((x) => {
        if (q.toLowerCase() === x.name.split(",")[0].toLowerCase()) {
          x.score = parseInt(x.score) + 0.7;
        } else {
          x.score = parseInt(x.score) + 0.3;
        }

        if (latitude) {
          if (
            x.latitude <= coordinateMax(latitude) &&
            x.latitude >= coordinateMin(latitude)
          ) {
            console.log(x);
            x.score = x.score + 0.15;
          }
        }

        if (longitude) {
          if (
            x.longitude <= coordinateMax(longitude) &&
            x.latitude >= coordinateMin(longitude)
          ) {
            x.score = x.score + 0.15;
          }
        }
      });

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
