
var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs').promises;

async function getCities() {
    const data = await fs.readFile('./data/cities_canada-usa.tsv', 'utf-8');  
    return cities = data.split('\n').map(city => ({
        id: city.split('\t')[0],
        name: city.split('\t')[2],
        latitude: city.split('\t')[4],
        longitude: city.split('\t')[5],
        countryCode: city.split('\t')[8],
        admin1: city.split('\t')[10],
        population: city.split('\t')[14]
      }));

};

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
//   console.log("suggestions", suggestions[0]);
//   return suggestions;
// }


async function filter(query) {
  const cities = await getCities();
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


module.exports = http.createServer(function(req, res) {
  res.statusCode = 404;
    if (req.url.indexOf('/suggestions') === 0) {
      async function query() {
        const parsedUrl = url.parse(req.url, true);
        const query = parsedUrl.query;
        return await filter(query);
      }
      query().then(suggestions => { 
        if (!suggestions) {
          res.statusCode = 404;
          res.end(
            JSON.stringify({
              suggestions: [],
            })
          );
        } else {
          res.statusCode = 200;
          res.end(
            JSON.stringify({
              suggestions: suggestions
            })
          );
        }})
    } else {
      res.statusCode = 500;
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
