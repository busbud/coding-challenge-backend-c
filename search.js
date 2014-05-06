var url = require('url');
var fs = require('fs');
var tsv = require('tsv');

// Conversion table for admin1 regions in Canada for name formatting
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

var loadCityData = function() {
    var tableText = fs.readFileSync('data/cities_canada-usa.tsv','utf-8');
    return tsv.parse(tableText);
}

var table = loadCityData();

var parseQuery = function(requestUrl) {
    parsedUrl = url.parse(requestUrl, true);
    return parsedUrl.query;
}

var lookup = function(query) {
    var q = query.q.toLowerCase();
    var res = [];
    table.forEach(function(element,index) {
        if(element.ascii != undefined &&
            element.ascii.toLowerCase().indexOf(q) != -1) {
            res.push(element);
        }
    });
    return res;
}

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

var sortByScore = function (a,b) {
    return b.score - a.score;
}

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
