
var json = require('../../data/cities_canada-usa.json')
var constants = require('./constants.js')
var strinScore = require('./getStringScore')
//console.log("diance", constants.CANADA_PROVINCES)
//console.log( strinScore.score("hello world","wor"))



//precomputation and validation 
let northAmericanCities = json.filter((el) => {
  return (el.country == 'CA' || el.country == 'US') && (el.population >= 5000)
})
console.log(json.length)
console.log(northAmericanCities.length)


function calculateOverallScore(distanceScore, stringScore){
  return (distanceScore + stringScore) / 2 
}

//Precomputation: request
let matches = json.filter((el) => {
  return el.name.indexOf('Londo') > -1 || el.alt_name.indexOf('Londo') > -1
})

console.log(matches.length)

//caculate the distance the results and sort them

function distance(lat1, lon1, lat2, lon2) {
	if ((lat1 == lat2) && (lon1 == lon2)) {
		return 0;
	}
	else {
		var radlat1 = Math.PI * lat1/180;
		var radlat2 = Math.PI * lat2/180;
		var theta = lon1 - lon2;
		var radtheta = Math.PI * theta/180;
		var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
		if (dist > 1) {
			dist = 1;
		}
		dist = Math.acos(dist);
		dist = dist * 180/Math.PI;
		dist = dist * 60 * 1.1515;
		return dist;
	}
}

matches.forEach(function(city){
  city['distance'] = distance(city["lat"],city["long"],43.70011, -79.4163)
})
//sorting
matches.sort((cityA, cityB) => cityB.distance - cityA.distance)

var maxDist = matches.length > 0 ? matches[0]['distance']: 0

console.log("max",maxDist)





/* for (i=matches.length-1;  i >= 0; i--){
  //console.log("score",i/matchesLen)
  //console.log("string",matches[i]['name'].score("Londo"))
  matches[i]['score'] = Math.round(calculateOverallScore(1 - i/matchesLen, matches[i]['name'].score("Londo")) * 100)/100
} */
//matches.sort((cityA, cityB) => cityB.score - cityA.score)
//console.log(matches)

var results = []

matches.forEach(function(city){
  city['score'] = Math.round(calculateOverallScore(1 - (city['distance']/maxDist), city['name'].score("Londo")) * 10)/10
  let region = city['country'] == 'CA' ? CANADA_PROVINCES[city['admin1']] : city['admin1']
  let country = city['country'] == 'CA' ? "Canada" :"USA"

  results.push(
    {
      "name": city["name"] + ', ' + region + ', ' + country,
      "latitude": city["lat"],
      "longitude": city["long"],
      "score": city['score']
    }
  )
})
results.sort((cityA, cityB) => cityB.score - cityA.score)

console.log(results)



CANADA_PROVINCES =
{
  "01": "AB",
  "02": "BC",
  "03": "MB",
  "04": "NB",
  "05": "NL",
  "07": "NS",
  "08": "ON",
  "09": "PE",
  "10": "QC",
  "11": "SK",
  "12": "YT",
  "13": "NT",
  "14": "NU" 
}