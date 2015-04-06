var geolib = require('geolib');


// function to score closeness of cities, both in their "name closeness"
// and their "physical distance closeness"
// If the user provides query latitude and logitude, we're going to weight
// the "physical distance closeness" with a higher priority (90%), since 
// all the names are already known to be fairly matching by prefix, the 
// % difference for lengths of the strings isn't super useful 
// compared to known distance

function score(query_city, query_lat, query_long, compare_city_info) {
    var name_closeness = query_city.length / compare_city_info.name.length
    
    // we need both latitude and longitude to calculate distance
    // if we dont have that info, use only the name closeness
    if (!query_lat || !query_long) {
        return name_closeness;
    }
    
    var distance = geolib.getDistance(
        {
            latitude: query_lat, 
            longitude: query_long
        }, {
            latitude: compare_city_info.lat, 
            longitude: compare_city_info.long 
        });
    
    // furthest possible distance between two points on earth
    // is half the earth circumference = 40075km/2 = 20037500m (source: google)
    // this ensures we don't get negative scores (dist/f.p.s can never be > 1)
    // an alternative would be to use furthest possible distance between two 
    // points within north america, but then care would need to be taken
    // to ensure input query coordinates are also within north america, 
    // otherwise scores might get below 0
    var FURTHEST_POSSIBLE_DISTANCE = 20037500;
    var distance_closeness = 1 - (distance/FURTHEST_POSSIBLE_DISTANCE)
    
    return (distance_closeness * 0.9) + (name_closeness * 0.1)
}

module.exports = score;