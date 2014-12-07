//Convert city search hit to formatted JSON string

exports.getFormattedObj = function (response_hit) {//Takes a search hit of form response_hit.city (city object), response_hit.score
    var city=response_hit.city;
    var obj = {};
    var formattedName = city.primary_name + ", " + city.state + ", " + fullCountryName(city.country);
    obj.name = formattedName;
    obj.latitude = city.lat;
    obj.longitude = city.long;
    obj.score = response_hit.score;
    return obj;
}
exports.getJSON = function (city, score) {
    return JSON.stringify(exports.getFormattedObj(city, score));
}

function fullCountryName(country_code) {
    switch (country_code) {
        case 'CA':
            return 'Canada';
        case 'US':
            return 'USA';
    }
}
