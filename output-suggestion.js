var regionsCanada = {
    "10" : "AB",
    "02" : "BC", 
    "03" : "MB", 
    "04" : "NB",
    "13" : "NT", 
    "07" : "NS",
    "14" : "NU", 
    "08" : "ON", 
    "09" : "PE",
    "10" : "QC", 
    "11" : "SK", 
    "12" : "YT", 
    "05" : "NL" 
}

function outputSuggestion (searchResults, inputLng, inputLat, query){
    var suggestions = [];
    for (city in searchResults) {
        var cityData = searchResults[city];
        var cityNameASCII = cityData["asciiname"];
        var cityLat = cityData["latitude"];
        var cityLng = cityData["longitude"];
        var admin1code = cityData["countrycode"] == "CA" ? regionsCanada[cityData["admin1code"]] : cityData["admin1code"];
        var cityObj = { 
            name: cityData["name"] + ", " + admin1code + " - " + cityData["countrycode"],
            latitude: cityLat,
            longitude: cityLng,
            score: confidenceScore( cityNameASCII, query, getDistanceFromLatLonInKm(cityLat, cityLng, inputLat, inputLng))
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



function confidenceScore ( cityNameASCII, query, distanceFromLatLon ){
    /* Part of score will be calculated using information of 
    * distance between latitude and longitude given in query 
    * and a city in the list.
    */
    var numberOfCharacters =  query.length / cityNameASCII.length;
    if( cityNameASCII.toLowerCase() === query.toLowerCase() ){
        return +(numberOfCharacters);
    }
   
    if(distanceFromLatLon === null ){
        return +(numberOfCharacters.toFixed(1));
    }
    if (distanceFromLatLon >= 0 ){
        var earthRadius = 6371;
        var distanceFromLatLon = 6371 - distanceFromLatLon;
        var score = distanceFromLatLon / 6371;
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