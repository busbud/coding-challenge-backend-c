const express = require("express");
const app = express();
const database_pool = require("./db/index");

const port = process.env.PORT || 2345;
// let params = {
//   q,
//   latitude,
//   longitude,
// };
// app.use(express.json());

//ROUTES
/*get all rows from the table*/
app.get("/", async (req, res, next) => {
  try {
    const allCities = await database_pool.query("SELECT *  FROM geoname");
    res.json(allCities);
  } catch (err) {
    next(err);
  }
});

//get suggestions
app.get("/suggestions", async (req, res, next) => {
  try {
    const suggestedCities = await database_pool.query(
      "SELECT country FROM geoname WHERE population > 5000 AND country IN ('US', 'CA')"
    );
    res.json(suggestedCities);
  } catch (err) {
    next(err);
  }
});

app.listen(port, () => {
  console.log("Server running at http://127.0.0.1:%d", port);
});
