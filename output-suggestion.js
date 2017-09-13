function outputSuggestion (searchResults, inputLng, inputLat, query){
    var suggestions = [];
    var query = query.toLowerCase();
   
    for (city in searchResults) {
        var cityData = searchResults[city];
        var cityLat = cityData["lat"];
        var cityLng = cityData["lng"];
        var forComparisonCityName = cityData["name"].toLowerCase();
        var cityObj = { 
            name: cityData["name"] + ", " + cityData["adminName1"] + " - " + cityData["countryCode"],
            latitude: cityLat,
            longitude: cityLng,
            score: confidenceScore(getDistanceFromLatLonInKm(cityLat, cityLng, inputLat, inputLng))
        };
        suggestions.push(cityObj);
    }
    return suggestions;
}

function confidenceScore ( distanceFromLatLon ){
    /* Part of score will be calculated using information of 
    * distance between latitude and longitude given in query 
    * and a city in the list.
    */
    var earthRadius = 6371;
    var score =   distanceFromLatLon / 6371;
    return score.toFixed(2);

    // Need to include calculation based on name. 
}

/* 
* Function to calculate km distance based on latitude provided. 
* The function itself was suggested in a Stackoverflow Q&A - URL: https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
* Considering I was not familiar with the Haversine formula, I have decided to use it practically as is, 
* instead of trying to replicate with my own code.
*/
function getDistanceFromLatLonInKm(cityLat,cityLng,inputLat,inputLng) {
    if(inputLat === null || inputLng == null ){
        return 0; 
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