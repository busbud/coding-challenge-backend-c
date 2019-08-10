const http = require('http');
const port = 8080;
const fs = require('fs');
const url = require('url');

fs.readFile('./data/cities_canada-usa.tsv', 'utf-8', function (err, data) {
    if (err) {
        throw err;
    }
    const dataSet = data.toString();
    filterData(dataSet);
});

let output = {};
// create a function that further cleans data
function filterData(input) {
  //convert string to array
  let cities = input.split('\n');

  let filteredList = cities.map(function(city){
    //each city's information is stored in an array
    let cityDetails = city.split('\t');

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
    let canada = "Canada";
    let usa = "USA";
    if (cityDetails[8] === "CA") {
      cityDetails[8] = canada;
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
    } else {
      cityDetails[8] = usa;
      // for US countries, each state is exactly admin1 code
      province = cityDetails[10];
    }
    //store each array's elements as a property of an object
    return {
      // id: cityDetails[0],
      name: cityDetails[1] + ", " + province + ", " + cityDetails[8],
      // ascii: cityDetails[2],
      latitude: cityDetails[4],
      longitude: cityDetails[5],
      // countryCode: cityDetails[8],
      // admin1: province,
      population: cityDetails[14],
      // timezone: cityDetails[17],
    }
  })
  
  //Remove accents
  filteredList.forEach(function(eachObj){
    const index = eachObj.name.indexOf(",")
    const restInfo = eachObj.name.substring(index)
    eachObj.name = accentsTidy(eachObj.name.substring(0, index)) + restInfo
  })

  //create output object
  output.suggestions = filteredList;
  return output;
}

// function that filters the filteredList from raw data based on query object

function suggestion (dataArray, queryObjInput) {
// functionalities:
// 1. find matched city name based on q attribute in query object
  if (queryObjInput.q) {

    const cityQuery = queryObjInput.q;
    const cityQueryLong = queryObjInput.longitude;
    const cityQueryLat = queryObjInput.latitude;

    let filteredResults = dataArray.filter((city) => 
    (city.population > 5000) && (city.countryCode == "CA" || "US") && 
    (city.name.normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase()
    .includes(cityQuery
      .normalize("NFD")
      .replace(/[\u0300-\u036f]/g, "")
      .toLowerCase())))

      // 2. calculate score based on name, longitude and latitude attribute in query object
      filteredResults.forEach(function(cityObj){

        // if query string is included in name string, city population is larger
        // than 5000, and in Canada or US, then get base score 0.5
        let score = 0.5;
        
        // if query string is at the beginning of name string, add 0.1 to the score
        if (cityQuery === cityObj.name.substring(0, cityQuery.length)) {
          score += 0.1;
        }

        // if query string is at the beginning of name string, and is at the same length
        // as city name, add 0.1 to the score
        if ((cityQuery.length === cityObj.name.indexOf(",")) && 
        (cityQuery === cityObj.name.substring(0, cityQuery.length))) {
          score += 0.1;
        }

        // if longitude difference is smaller than 1, then add 0.15 to the score
        if (cityQueryLong) {

          if (Math.abs(cityQueryLong - cityObj.longitude) < 1) {
            score += 0.15;
          }

        }

        // if latitdue difference is smaller than 1, then add 0.15 to the score
        if (cityQueryLat) {

          if (Math.abs(cityQueryLat - cityObj.latitude) < 1) {
            score += 0.15;
          }

        }
        // 3. store the score in the returned array of results
        cityObj.score = score;

      })
    // 4. return all relevant information sorted by score (descending order)
    const sortedFilteredResults = filteredResults.sort(function(a, b){return b.score - a.score})

    return sortedFilteredResults;
  }

}

// from https://stackoverflow.com/questions/990904/remove-accents-diacritics-in-a-string-in-javascript
accentsTidy = function(r){
  // let r=s.toLowerCase();
  r = r.replace(new RegExp(/\s/g),"");
  r = r.replace(new RegExp(/[àáâãäå]/g),"a");
  r = r.replace(new RegExp(/æ/g),"ae");
  r = r.replace(new RegExp(/ç/g),"c");
  r = r.replace(new RegExp(/[èéêë]/g),"e");
  r = r.replace(new RegExp(/[ìíîï]/g),"i");
  r = r.replace(new RegExp(/ñ/g),"n");                
  r = r.replace(new RegExp(/[òóôõö]/g),"o");
  r = r.replace(new RegExp(/œ/g),"oe");
  r = r.replace(new RegExp(/[ùúûü]/g),"u");
  r = r.replace(new RegExp(/[ýÿ]/g),"y");
  r = r.replace(new RegExp(/\W/g),"");
  return r;
};

module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    const parsedUrl = url.parse(req.url, true);
    const queryObj = parsedUrl.query;
    // if queryObj is empty
    if (Object.getOwnPropertyNames(queryObj).length === 0) {
      res.end("You don't have any search input")
    } else {
      const suggestions = suggestion(output.suggestions, queryObj);
      //check whether array is empty
      if (suggestions.length < 1){
        res.writeHead(404, {'Content-Type': 'text/plain'});
          res.end(
            JSON.stringify({
              suggestions
            })
          );

      } else if (suggestions.length >= 1 ){
        res.statuscode = 200;
        res.end(JSON.stringify({
          suggestions
        }));

      } else {
        res.end();
      }
    }
  }

}).listen(port);

console.log(`Server running at http://localhost:${port}/suggestions`);



// Original code of planning on using express server and deliver ejs templates in frontend

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