// const express = require("express");
// const app = express();
// const sass = require("node-sass-middleware");
// app.set("view engine", "ejs");
// app.use(
//   "/styles",
//   sass({
//     src: __dirname + "/styles",
//     dest: __dirname + "/public/styles",
//     debug: true,
//     outputStyle: "expanded"
//   })
// );
// app.use(express.static(__dirname + "/public"));
// PORT = 3000;




// app.get("/", (req, res) => {
//   res.send("hello! Please go to /suggestions");
// });


// app.listen(PORT, () => {
//   console.log('Server running at localhost:%d/suggestions', PORT);
// });


// module.exports = app;

////////////////////////////////////////////////////////////////////////////
const http = require('http');
const port = 8080;
const fs = require('fs');
const url = require('url');

fs.readFile('./data/cities_canada-usa.tsv', 'utf-8', function (err, data) {
    if (err) {
        throw err;
    }
    const dataSet = data.toString();
    console.log(filterData(dataSet))
});

// create a function that further cleans data
function filterData(input) {
  //convert string to array
  cities = input.split('\n')
  let filteredList = cities.map(function(city){
    //each city's information is stored in an array
    const cityDetails = city.split('\t')
    // According to http://download.geonames.org/export/dump/admin1CodesASCII.txt
    // and http://www.comeexplorecanada.com/abbreviations.php
    // CA.01	Alberta	Alberta	5883102
    // CA.02	British Columbia	British Columbia	5909050
    // CA.03	Manitoba	Manitoba	6065171
    // CA.04	New Brunswick	New Brunswick	6087430
    // CA.13	Northwest Territories	Northwest Territories	6091069
    // CA.07	Nova Scotia	Nova Scotia	6091530
    // CA.14	Nunavut	Nunavut	6091732
    // CA.08	Ontario	Ontario	6093943
    // CA.09	Prince Edward Island	Prince Edward Island	6113358
    // CA.10	Quebec	Quebec	6115047
    // CA.11	Saskatchewan	Saskatchewan	6141242
    // CA.12	Yukon	Yukon	6185811
    // CA.05	Newfoundland and Labrador	Newfoundland and Labrador	6354959

    // Change each province's administration code to abbreviations
    let province = "";
    if (cityDetails[8] === "CA") {
      switch (cityDetails[10]) {
        case "01":
          province = "AB"
          break;
        case "02":
          province = "BC"
          break;
        case "03":
          province = "MB"
          break;
        case "04":
          province = "NB"
          break;
        case "05":
          province = "NL"
          break;
        case "07":
          province = "NS"
          break;
        case "08":
          province = "ON"
          break;
        case "09":
          province = "PE"
          break;
        case "10":
          province = "QC"
          break;
        case "11":
          province = "SK"
          break;
        case "12":
          province = "YT"
          break;
        case "13":
          province = "NT"
          break;
      }
    }
    //store each array's elements as a property of an object
    return {
      id: cityDetails[0],
      name: cityDetails[1],
      ascii: cityDetails[2],
      latitude: cityDetails[4],
      longitude: cityDetails[5],
      countryCode: cityDetails[8],
      admin1: province,
      population: cityDetails[14],
      timezone: cityDetails[17],
    }
  })
  //create output object
  const output = {}
  output.suggestions = filteredList
  return output
}



module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    const parsedUrl = url.parse(req.url, true);
    const query = parsedUrl.query;
    console.log(parsedUrl)
    console.log(query)
    console.log(query.q)
    // res.end(JSON.stringify({
    //   suggestions: []
    // }));
  } else {
    res.end();
  }
}).listen(port);

console.log(`Server running at http://localhost:${port}/suggestions`);