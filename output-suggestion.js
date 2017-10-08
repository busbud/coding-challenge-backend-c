/*
* This is a manually created list of abrreviated regions 
* in Canada. The purpose is to ensure consistency with US states. 
* If a new region is created, it should be added to the object below. 
* Regions were taken from the following table: 
* http://www.geonames.org/CA/administrative-division-canada.html
*/
var regionsCanada = {
    "01" : "AB",
    "02" : "BC", 
    "03" : "MB", 
    "04" : "NB",
    "05" : "NL", 
    "07" : "NS",
    "08" : "ON",
    "09" : "PE",
    "10" : "QC", 
    "11" : "SK", 
    "12" : "YT", 
    "13" : "NT", 
    "14" : "NU"
}

/*  
* This is the main function used in the API. Its purpose is to loop over results 
* and organize the data in the appropriate way. 
*
* If there is a match that returns 1, then only that result should be returned. 
* 
* Asciiname column was used instead of name to avoid outputting characters with accent
* admin1code is the region (Canada) or state (US)
* countrycode is the abbreviation of country
*
*/

function outputSuggestion (searchResults, inputLng, inputLat, query){
    var suggestions = [];
    for (city in searchResults) {
        var cityData = searchResults[city];
        var cityName = cityData["asciiname"];
        var cityLat = cityData["latitude"];
        var cityLng = cityData["longitude"];
        var admin1code = cityData["countrycode"] == "CA" ? regionsCanada[cityData["admin1code"]] : cityData["admin1code"];
        var cityObj = { 
            name: cityName + ", " + admin1code + " - " + cityData["countrycode"],
            latitude: cityLat,
            longitude: cityLng,
            score: confidenceScore( cityName, query, getDistanceFromLatLonInKm(cityLat, cityLng, inputLat, inputLng))
        };
        suggestions.push(cityObj);
    }
    var sortedSuggestions = suggestions.sort( function(a, b){  return b["score"]-a["score"] });
    var matchedSuggestion = sortedSuggestions.filter(function(city){ return city["score"] == 1 });

    if ( matchedSuggestion.length == 1){
        return matchedSuggestion;
    } else {
        return sortedSuggestions;
    }
}


/* 
* Function: Confidence Score 
* Output: Number with only one decimal 
*
* The score is calculated as follows: 
* The first calculation is the number of characters in the query divided 
* by the number of characters in the city name. If cityname is the same as 
* the query, then number to be returned is 1 as it should be a match. 
* 
* If it's not a match, and latitude and longitude are also not provided, 
* then the calculation will be returned. 
*
* If latitude and longitude are provided, the distanceFromLatLon
* is used to calculate how far the coordinates are of a certain city.
*/
function confidenceScore ( cityName, query, distanceFromLatLon ){
    var numberOfCharacters =  query.length / cityName.length;
    if( cityName.toLowerCase() === query.toLowerCase() ){
        return +(numberOfCharacters);
    }
   
    if(distanceFromLatLon === null ){
        return +(numberOfCharacters.toFixed(1));
    }
    if (distanceFromLatLon >= 0 ){
        var earthRadius = 6371;
        var distanceFromLatLon = 6371 - distanceFromLatLon;
        var distanceScore = distanceFromLatLon / 6371;
        var score = (numberOfCharacters * 2 + distanceScore * 8 ) / 10;
        return +(score.toFixed(1));
       
    }
}

/* 
* Function to calculate km distance based on latitude provided. 
* The function itself was suggested in a Stackoverflow Q&A - URL: https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
* Considering I was not familiar with the Haversine formula, I have decided to use it practically as is, 
* instead of trying to replicate with my own code.
*/
function getDistanceFromLatLonInKm(cityLat,cityLng,inputLat,inputLng) {
    if(inputLat === null || inputLng == null ){
        return null; 
    }
    var Radius = 6371; // Radius of the earth in km

    var dLat = deg2rad(inputLat-cityLat);  // deg2rad below
    var dLon = deg2rad(inputLng-cityLng); 
    var a = 
      Math.sin(dLat/2) * Math.sin(dLat/2) +
      Math.cos(deg2rad(cityLat)) * Math.cos(deg2rad(inputLat)) * 
      Math.sin(dLon/2) * Math.sin(dLon/2)
      ; 
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
    var d = Radius * c; // Distance in km
    return d;
  }
  
  function deg2rad(deg) {
    return deg * (Math.PI/180);
  }
  
module.exports = outputSuggestion;