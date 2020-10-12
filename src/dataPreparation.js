//conversion tsv file to json
const fs = require("fs");
const d3 = require("d3");
let tsvData = "./data/cities_canada-usa.tsv";
var rawData = require('./data/cities_canada-usa.json')

let jsonData = [];
fs.readFile(tsvData, "utf-8", function (error, data) {
  data = d3.tsvParse(data);
  var jsonData = JSON.stringify(data);
  fs.writeFileSync("./data/cities_canada-usar.json", jsonData);

  //precomputation and data validation
  var data = rawData.filter((el) => {
    return (el.country == "CA" || el.country == "US") && el.population >= 5000;
  });
  fs.writeFileSync("./data/cities_canada-usar.json", data);
});
