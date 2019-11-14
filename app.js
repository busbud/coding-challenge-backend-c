const path = require("path");
const express = require("express");
const _ = require("lodash");

const { makeScorer } = require("./score.js");
const { parsecsv } = require("./parsecsv.js");

// constants
const PORT            = process.env.PORT || 8080;
const SCORE_THRESHOLD = process.env.SCORE_THRESHOLD || 0.20;
const RESULTS_QTY     = process.env.RESULTS_QTY || 6;
const distWeight      = process.env.distWeight || 0.42;
const nameWeight      = process.env.nameWeight || 0.62;
const popWeight       = process.env.popWeight || 0.06;

// objects
const app = express();
const records = parsecsv(path.join(__dirname, "data", "cities_canada-usa.tsv"));
const scorerConfig = {
  distWeight, nameWeight, popWeight,
  maxPop: _.maxBy(records, "population")["population"]
};

app.get("/suggestions", (req, res) => {
  const query = {
    q:         req.query.q,
    longitude: req.query.longitude,
    latitude:  req.query.latitude,
  };

  if (!query.q) {
    res.json({ suggestions: [] });
    return;
  }

  const scorer = makeScorer(scorerConfig, query);
  const results = _.chain(records)
    .map( r => ({ score: scorer(r), ...r }))
    .filter(r => r["score"] > SCORE_THRESHOLD)
    .sortBy("score")
    .reverse()
    .take(RESULTS_QTY)
    .value();

  const httpStatus = results.length ? 200 : 404;
  res.status(httpStatus).json({ suggestions: results });
});

app.listen(PORT, "0.0.0.0", () => {
  console.log(`server running on port ${PORT}`);
});

module.exports = app;
