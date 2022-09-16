const express = require("express");
const app = express();
const fs = require("fs");
const tsv = require("tsv");

levenshteinDistanceScore = require('./utils/levenshteinDistance.js');
getDataWithScores = require('./utils/scores.js');
getProvinceCode = require('./utils/provinceCodes.js');

var port = process.env.PORT || 2345;

var parsedData = {};
/*
  Reading data when api starts to run,
  therefore locations data is stored in the memory 
  and we don't need to read it from file everytime
*/
fs.readFile("./data/cities_canada-usa.tsv", "utf8", function (error, data) {
  parsedData = tsv
    .parse(data)
    //decided to filter for the rules after data is parsed
    .filter(
      (item) =>
        item.id !== "" &&
        item.population >= 5000 &&
        (item.country == "CA" || item.country == "US")
    )
    .map((item) => {
      return {
        id: item.id,
        name: item.ascii,
        country: item.country,
        latitude: item.lat,
        longitude: item.long,
        admin1: getProvinceCode(item.admin1),
      };
    });
});

app.get("/", (req, res) => {
  res.end(
    JSON.stringify("Busbud Coding Challenge")
  );
});

app.get("/suggestions", (req, res) => {
  const params = {
    filterWord: req.query.q,
    latitude: req.query.latitude && Number(req.query.latitude),
    longitude: req.query.longitude && Number(req.query.longitude),
  };

  const filteredData =
    params.filterWord && filterDataByKeyword(parsedData, params.filterWord);

  res.end(
    JSON.stringify({
      suggestions: filteredData ? getDataWithScores(filteredData, params) : [],
    })
  );
});

app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}/`);
});

//Decided to filter data by the keyword using includes so the list we got to work on just gets a bit shorter
function filterDataByKeyword(parsedData, keyWord) {
  return parsedData
    .filter((item) => item.name.toLowerCase().includes(keyWord.toLowerCase()))
    .map((item) => {
      return {
        id: item.id,
        name: item.name,
        country: item.country,
        latitude: item.latitude,
        longitude: item.longitude,
        admin1: item.admin1,
      };
    });
}

module.exports = app;
