//Get cities that potentially match a search term

exports.getMatches = function(city_container, search_term) {//Returns an array of city objects that may match.
    //Designing against blob of cities straight from citiesParser
    matches = [];
    for (key in city_container) {
        var city = city_container[key];
        //TODO: Filter out small cities (even though all entries in current database seem to be >5k pop.)
        if (isPotentialMatch(city.primary_name, search_term)) {
            matches.push(city);
        }
    }
    return matches;
};

function isPotentialMatch(hit_string, search_term) {//Returns true if hitString (string associated with an entry) is a potential match for searchTerm
    return hit_string.substring(0,search_term.length).toLowerCase() === search_term.toLowerCase();//Could be improved
}