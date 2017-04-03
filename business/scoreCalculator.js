var es = require("event-stream");
var haversine = require("haversine");

/**
 * Compute the score for each city depending on the search term
 * @param {*} cities - a list of cities for which a score must be calculated
 * @param {*} searchTerm - the search term
 * @param {Object} location - a {latitude, longitude} object
 */
function computeScores(cities, searchTerm, location) {
    var refLength = searchTerm.length;
    
    var reader = es.readArray(cities)            
            .pipe(es.mapSync(function(city) {
                var distanceScore = 0;
                if (location) {
                    city.distance = haversine(location, { latitude: city.latitude, longitude: city.longitude });
                    // 0 to 10 km is the best score, after we decrease by 0.5% each km
                    if (city.distance <= 10)
                        distanceScore = 100;
                    else
                        distanceScore = 100 - Math.min(90, ((city.distance - 10) / 2));
                }
                
                var nameScore = 100;
                var cityNameLength = city.name.length;
                if (cityNameLength > refLength) {
                    nameScore = 100 - (10 * Math.min(9, cityNameLength - refLength));
                }

                city.score = distanceScore
                    ? ((nameScore * 2) + (distanceScore * 8)) / 10 // name matching counts for 20% in the score, distance for 80%
                    : nameScore; // if no initial location has been provided, only the name score counts
                
                city.score = (city.score / 100).toFixed(1);
                return city;
            }));
    return reader;
}

module.exports.computeScores = computeScores;