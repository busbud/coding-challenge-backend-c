var url = require('url');
var fs = require('fs');
var tsv = require('tsv');

// Conversion cityData for admin1 regions in Canada for name formatting
var admin1ConversionCA = {
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

/**
 * Load the city TSV text file and turn it into a JSON object.
 *
 * @return {Object}
 *
 */
var loadCityData = function() {
    var tableText = fs.readFileSync('data/cities_canada-usa.tsv','utf-8');
    return tsv.parse(tableText);
}

var cityData = loadCityData();

/**
 * Parse an url and retrieve the search portion as a JSON object
 *
 * @param {String} requestUrl - URL to retrieve query from
 * @return {Object}
 *
 */
var parseQuery = function(requestUrl) {
    parsedUrl = url.parse(requestUrl, true);
    return parsedUrl.query;
}

/**
 * Checks if parsed query contains a member q and goes through all the TSV rows
 * if they contain matching characters in the ascii member
 *
 * @param {Object} query - Query object used to search for matching city names
 * @return {Array}
 *
 */
var lookup = function(query) {
    var q = query.q.toLowerCase();
    var res = [];
    cityData.forEach(function(element,index) {
        if(element.ascii != undefined &&
            element.ascii.toLowerCase().indexOf(q) != -1 &&
            element.population > 5000) {
            res.push(element);
        }
    });
    return res;
}

/**
 * Takes a city object and returns a smaller object with only longitude, latitude,
 * score and prettified name containing city name, state or province and country
 *
 * @param {Object} city - City object that contains the row information from the TSV
 * @return {Object}
 *
 */
var format = function(city) {
    var formatted = {};

    var stateProvince = city.admin1;
    var countryName = city.country;

    if(city.country == "CA")
    {
        // Fix for turning the numerical admin region back into a string.
        var code = "" + city.admin1;
        if(code.length < 2) code = "0" + code;

        stateProvince = admin1ConversionCA[code];
        countryName = "Canada";
    }
    else if(city.country == "US")
    {
        countryName = "USA";
    }

    formatted.name = city.ascii + ", " + stateProvince + ", " + countryName;
    formatted.latitude = city.lat;
    formatted.longitude = city.long;
    formatted.score = city.score;

    return formatted;
}

/**
 * Scores the cities in the cities array according to how closely the name matches
 * as well as coordinates if present. Returns an array of formatted city objects.
 *
 * @param {Array} cities - City object that contains the row information from the TSV
 * @param {Object} query - Query object used to search for matching city names
 * @return {Array}
 *
 */
var score = function(cities,query) {
    var res = [];
    cities.forEach(function(element) {
        var len = element.ascii.length;
        var qLen = query.q.length;
        var score = qLen/len;

        var long = (query.long || query.longitude);
        if(long)
        {
            var longScore = (360 - Math.abs(element.long - long)) / 360;
            score += longScore;
            score /= 2;
        }

        var lat = (query.lat || query.latitude);
        if(lat)
        {
            var latScore = (180 - Math.abs(element.lat - lat)) / 180;
            score += latScore;
            score /= 2;
        }

        element.score = score;

        var city = format(element);

        res.push(city);
    });

    return res;
}

// Standard greatest integer sort function, but uses score member.
var sortByScore = function (a,b) {
    return b.score - a.score;
}

/**
 * Search for a city based on requestUrl. Returns array of cities.
 *
 * @param {String} requestUrl - URL to retrieve query from
 * @return {Array}
 *
 */
exports.search = function(requestUrl) {
    var query = parseQuery(requestUrl);
    var suggestions = [];
    if(query.q != undefined)
    {
        var cities = lookup(query);
        suggestions = score(cities,query);
        suggestions.sort(sortByScore);
    }
    return suggestions;
}
