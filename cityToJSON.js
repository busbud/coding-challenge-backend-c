//Convert city object to formatted JSON string

getFormattedObj = function (city, score) {
    var obj = {};
    var formattedName = city.primary_name + ", " + city.state + ", " + fullCountryName(city.country);
    obj.name = formattedName;
    obj.latitude = city.lat;
    obj.longitude = city.long;
    obj.score = score;
    return obj;
}
exports.getJSON = function (city, score) {
    return JSON.stringify(getFormattedObj(city, score));
}

function fullCountryName(country_code) {
    switch (country_code) {
        case 'CA':
            return 'Canada';
        case 'US':
            return 'USA';
    }
}
