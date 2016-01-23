var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;
var fs = require('fs');
var es = require('event-stream');
var extend = require('util')._extend;

console.log('Server running at http://127.0.0.1:%d/suggestions', port);

cityDatabase = readCityFile('data/cities_canada-usa.tsv');

module.exports = http.createServer(function (req, res) {
  var parsedUrl = url.parse(req.url, true);
  var pathnameReq = parsedUrl.pathname;
  var citynameReq = parsedUrl.query.q;
  var latReq = parsedUrl.query.latitude;
  var longReq = parsedUrl.query.longitude;
  
  // if the URL and query is legal
  if (pathnameReq === '/suggestions' && citynameReq != null) {
    var cityInput = {};
    cityInput['name'] = citynameReq;
    cityInput['latitude'] = latReq;
    cityInput['longitude'] = longReq;
    resCities = matchedCities(cityInput, cityDatabase);

    // 404 response if no match
    if (Object.keys(resCities).length === 0) {
      res.writeHead(404, {'Content-Type': 'text/plain'});
      res.end(JSON.stringify({
        suggestions: []
      }));
    }
    // 200 repsonse if there is a match
    else {
      sortedCities = resCities.sort(function(a, b){
        if (a['score'] > b['score'])
          return -1;
        else if (a['score'] === b['score'])
          return 0;
        else 
          return 1;
      });
      res.writeHead(200, {'Content-Type': 'text/plain'});
      res.end(JSON.stringify({
        suggestions: sortedCities
      }));
    }
  // 404 response for illegal request
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end("Invalid request");
  }
}).listen(port, '127.0.0.1');

/**
 * Read all cities into memory from the table.
 *
 * @param {String} file - filename of the data table.
 * @return {Array} Array containing all cities listed in the table.
 */
function readCityFile(file) {
  var cities = {};
  fs.createReadStream(file)
    .pipe(es.split('\n'))
    .pipe(es.mapSync(function(data) { 
      return data.split("\t"); }))
    .pipe(es.mapSync(function(data) {
      cities[data[0]]={}; 
      cities[data[0]]['name'] = data[1]; 
      cities[data[0]]['ascii'] = data[2]; 
      cities[data[0]]['latitude'] = data[4]; 
      cities[data[0]]['longitude'] = data[5];
      cities[data[0]]['country'] = data[8];
      cities[data[0]]['province'] = data[10];
     }))
    .pipe(es.wait(function(){
      delete cities[''];
      delete cities['id'];
    }));
    return cities;
}

/**
 * Find all cities whose name has a prefix of the string in query
 *
 * @param {object} cityInput - city created from query
 * @param {String} cities - all cities in the data table
 * @return {Array} Array of all suggestions in the required format.
 */
function matchedCities(cityInput, cities) {
  var matchedCities = [];
  for (curCity in cities) {
    var cityScore = calcSimilarityScore(cityInput, cities[curCity]);
    if (cityScore === 0)
      continue;
    var newSimilarCity = extend({}, cities[curCity]);
    if (newSimilarCity['country'] === 'CA')
      newSimilarCity['name'] = newSimilarCity['ascii'] + ', ' + canadianProvince(newSimilarCity['province']) + ', ' + 'CA';
    if (newSimilarCity['country'] === 'US')
      newSimilarCity['name'] = newSimilarCity['ascii'] + ', ' + newSimilarCity['province'] + ', ' + 'US';
    newSimilarCity['score'] = cityScore;
    delete newSimilarCity['ascii'];
    delete newSimilarCity['country'];
    delete newSimilarCity['province'];
    matchedCities.push(newSimilarCity);
  }
                                                                                  
  return matchedCities;
}


/**
 * Check if a string is a prifix of another string.
 *
 * @param {String} partialName - name from query
 * @param {String} fullName - full city name
 * @return {Boolean} True if partialName is the prifix of fullName. Case insensitive.
 */
function isPartialName(partialName, fullName) {
  if (partialName.toLowerCase() === fullName.slice(0, partialName.length).toLowerCase())
    return true;
  return false;
}

/**
 * Calculating the similarity score of two cities.
 *
 * @param {object} cityInput - city created from query
 * @param {object} cityDB - city which is used to compare with cityInput
 * @return {Number} the score, how similar is cityDB to cityInput
 */
function calcSimilarityScore(cityInput, cityDB) {
  if (!isPartialName(cityInput['name'], cityDB['name']) && !isPartialName(cityInput['name'], cityDB['ascii'])) 
    return 0;

  var commonPrefixScore = cityInput['name'].length/cityDB['name'].length;

  // Note: in order to use longitude and latitude information, both longitude and latitude
  // must be provided in the query. If only one of them presents, it will be ignored.
  if (cityInput['longitude'] && cityInput['latitude']) {
    var longDiff = Math.abs(Math.abs(cityInput['longitude']) - Math.abs(cityDB['longitude']));
    var latDiff = Math.abs(Math.abs(cityInput['latitude']) - Math.abs(cityDB['latitude']));
    var positionScore = 1/(latDiff + longDiff);
    return (commonPrefixScore + positionScore)/2;
  } else {
    return commonPrefixScore;
  }
}

/**
 * Find Canadian province abbreviations according to the code in the table.
 *
 * @param {string} code in the data table
 * @return {string} abbreviations of Canadian province
 */
function canadianProvince(code){
  switch (code){
    case '01':
      return "AB";
      break;
    case '02':
      return "BC";
      break;
    case '03':
      return "MB";
      break;
    case '04':
      return "NB";
      break;
    case '05':
      return "NL";
      break;
    case '07':
      return "NV";
      break;
    case '08':
      return "ON";
      break;
    case '09':
      return "PE";
      break;
    case '10':
      return "QC";
      break;
    default:
      return "";
  }
}

/*
* First tried to use longest common prefix to match city names,
* that would end up with too many suitble cities, then I change
* it to find cities with prifix that contains the entire query,
* which is also what is shown in the example.
*
function longestCommonPrefixSize(str1, str2) {
  var prefixSize = 0;
  if (str1.length === 0 || str2.length === 0 || !str1 || !str2)
    return prefixSize;
  for (i = 0; i < str1.length; ++i) {
    if (i >= str2.length || str1[i] != str2[i])
      return prefixSize;
    prefixSize++;
  }
  return prefixSize;
}
*/