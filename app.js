// var http = require('http');
// var port = process.env.PORT || 2345;
// const url = require('url');
// const fs = require('fs');

// fs.readFile('./data/cities_canada-usa.tsv', 'utf-8', function (err, data) {
//     if (err) {
//         throw err;
//     }
//     const content = data.toString();
//     processFile(content);
// });

// let cities = [];

// function processFile(content) {

//   cities = content.split('\n').map(city => {
//     const info = city.split('\t');
//     return {
//       id: info[0],
//       name: info[1],
//       // asciiname: info[2],
//       // alternatenames: info[3],
//       latitude: info[4],
//       longitude: info[5],
//       // featureClass: info[6],
//       // featureCode: info[7],
//       countryCode: info[8],
//       // cc2: info[9],
//       admin1: info[10],
//       // admin2: info[11],
//       // admin3: info[12],
//       // admin4: info[13],
//       population: info[14],
//       // elevation: info[15],
//       // dem: info[16],
//       // timezone: info[17],
//       // modDate: info[18],
//     };
//   });

//   for (i=0; i<3; i++) {
//    switch(cities[i].admin1){
//    case "01":
//      cities[i].name = `${cities[i].name}, Alberta`
//      break;
//     case "10":
//       cities[i].name = `${cities[i].name}, Quebec`
//       break;
//   }}

//   return cities;
// }

// //http://download.geonames.org/export/dump/admin1CodesASCII.txt
// // CA.02	British Columbia	British Columbia	5909050
// // CA.03	Manitoba	Manitoba	6065171
// // CA.04	New Brunswick	New Brunswick	6087430
// // CA.13	Northwest Territories	Northwest Territories	6091069
// // CA.07	Nova Scotia	Nova Scotia	6091530
// // CA.14	Nunavut	Nunavut	6091732
// // CA.08	Ontario	Ontario	6093943
// // CA.09	Prince Edward Island	Prince Edward Island	6113358
// // CA.10	Quebec	Quebec	6115047
// // CA.11	Saskatchewan	Saskatchewan	6141242
// // CA.12	Yukon	Yukon	6185811
// // CA.05	Newfoundland and Labrador	Newfoundland and Labrador	6354959

// function filter(cities, query) {
//   const filteredCities = cities.filter((city) => (city.population > 5000) && (city.countryCode == "US" || "CA") && (city.name.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase().includes(query.q.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase())))
//   for (i = 0; i < filteredCities.length; i++) {
//     filteredCities[i].score = 0.1;
//     if (query.q.toLowerCase() == filteredCities[i].name.toLowerCase()) {
//       filteredCities[i].score = 0.8;
//     }
//     if (query.latitude) {
//       if (parseFloat(query.latitude) - parseFloat(filteredCities[i].latitude) < 1) {
//         filteredCities[i].score += 0.1;
//       }
//     }
//     if (query.longitude) {
//       if (parseFloat(query.longitude) - parseFloat(filteredCities[i].longitude) < 1) {
//         filteredCities[i].score += 0.1;
//       }
//     }
//   }
//   const sortedSuggestions = filteredCities.sort((a,b) => (b.score - a.score));
//   return sortedSuggestions;
// }

// module.exports = http.createServer(function (req, res) {
//   // res.writeHead(200, {'Content-Type': 'text/plain'});

//   if (req.url.indexOf('/suggestions') === 0) {
//     const parsedUrl = url.parse(req.url, true);
//     const query = parsedUrl.query;
//     const suggestions = filter(cities, query);
//     console.log(suggestions);
//     if (suggestions.length < 1) {
//       res.statuscode = 404;
//       res.end(JSON.stringify({
//         suggestions: [],
//       }));
//     } else {
//       res.statusCode = 200;
//       res.end(JSON.stringify({
//         suggestions: suggestions,
//       }));
//     }
//   } else {
//     res.end();
//   }
// }).listen(port, '127.0.0.1');

// console.log('Server running at http://127.0.0.1:%d/suggestions', port);

var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs');

const response = {};

fs.readFile('./data/cities_canada-usa.tsv', 'utf-8', function(err, data) {
  if (err) {
    throw err;
  }

  const content = data.toString();
  processFile(content);
});


function processFile(content) {
  response.suggestions = content.split('\n').map(city => ({
    id: city.split('\t')[0],
    name: city.split('\t')[2],
    latitude: city.split('\t')[4],
    longitude: city.split('\t')[5],
    countryCode: city.split('\t')[8],
    admin1: city.split('\t')[10],
    population: city.split('\t')[14]
  }));

  // for (i = 0; i < (response.suggestions.length - 1); i++) {
    // switch (response.suggestions[i].admin1) {
    //   case '01':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Alberta`;
    //     break;
    //   case '02':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, British Columbia`;
    //     break;
    //   case '03':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Manitoba`;
    //     break;
    //   case '04':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, New Brunswick`;
    //     break;
    //   case '05':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Newfoundland and Labrador`;
    //     break;
    //   case '07':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Nova Scotia`;
    //     break;
    //   case '08':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Ontario`;
    //     break;
    //   case '09':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, PEI`;
    //     break;
    //   case '10':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Quebec`;
    //     break;
    //     case '11':
    //     response.suggestions[i].name = `${
    //       response.suggestions[i].name
    //     }, Saskatchewan`;
    //     break;
    //     case '12':
    //       response.suggestions[i].name = `${
    //         response.suggestions[i].name
    //       }, Yukon`;
    //     break;
    //     case '13':
    //       response.suggestions[i].name = `${
    //         response.suggestions[i].name
    //       }, Northwest Territories`;
    //     break;
    //     case '14':
    //       response.suggestions[i].name = `${
    //         response.suggestions[i].name
    //       }, Nunavut`;
    //     break;
    // }
  // }
  return response;
}


function filter(cities, query) {
  query.q = query.q.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  const filteredCities = cities.filter(
    city =>
    city.population > 5000 &&
    (city.countryCode == 'US' || 'CA') &&
      city.name.toLowerCase().includes(query.q)
  );
  for (i = 0; i < filteredCities.length; i++) {
    filteredCities[i].score = 0.1;
    if (query.q == filteredCities[i].name.toLowerCase()) {
      filteredCities[i].score += 0.5;
    }
    if (filteredCities[i].population > 100000) {
      filteredCities[i].score += 0.1;
    }
    if (query.q[0] == filteredCities[i].name[0].toLowerCase()) {
      filteredCities[i].score += 0.1;
    }
    if (query.latitude) {
      if (
        parseFloat(query.latitude) - parseFloat(filteredCities[i].latitude) <
        1
      ) {
        filteredCities[i].score += 0.1;
      }
    }
    if (query.longitude) {
      if (
        parseFloat(query.longitude) - parseFloat(filteredCities[i].longitude) <
        1
      ) {
        filteredCities[i].score += 0.1;
      }
    }
  }
  const sortedSuggestions = filteredCities.sort((a, b) => b.score - a.score);
  return sortedSuggestions;
}


module.exports = http.createServer(async function(req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
      const parsedUrl = url.parse(req.url, true);
      const query = parsedUrl.query;
      const suggestions = await filter(response.suggestions, query);
      console.log(suggestions);
      if (!suggestions) {
        res.statusCode = 404;
        res.end(
          JSON.stringify({
            suggestions
          })
        );
      } else {
        res.statusCode = 200;
        res.end(
          JSON.stringify({
            suggestions
          })
        );
      }
      console.log(res.json);
    } else {
      // res.statusCode = 500;
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
