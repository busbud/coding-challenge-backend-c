var es = require("event-stream");
var haversine = require("haversine");

/**
 * Compute the score for each city depending on the search term
 * @param {Array} cities - a list of cities for which a score must be calculated
 * @param {string} searchTerm - the search term
 * @param {Object} location - a {latitude, longitude} object
 */
function computeScores(cities, searchTerm, location) {
    var refLength = searchTerm.length;
    
    var reader = es.readArray(cities)            
            .pipe(es.mapSync(function(city) {
                
                city.score = 100;

                if (location) {
                    city.distance = haversine(location, { latitude: city.latitude, longitude: city.longitude });
                    // 0 to 10 km is the best score, after we decrease by 0.2% each km (eg 50km = 10% off)
                    if (city.distance > 10)
                        city.score = 100 - Math.min(90, (city.distance / 5));
                }
                
                var cityNameLength = city.name.length;
                if (cityNameLength > refLength) {
                    // each extra character lowers the score by 2% or 5% (depending wether a location is specified or not)
                    var lossFactor = location ? 2 : 5;
                    var coeff = 100 - (lossFactor * Math.min(9, cityNameLength - refLength));
                    city.score = city.score * (coeff / 100);
                }

                city.score = (city.score / 100).toFixed(1);

                return city;
            }));
    return reader;
}

module.exports.computeScores = computeScores;