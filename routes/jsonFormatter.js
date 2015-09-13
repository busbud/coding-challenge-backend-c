var FuzzySearch = require('fuzzysearch-js');
var levenshteinFS = require('fuzzysearch-js/js/modules/LevenshteinFS');
var indexOfFS = require('fuzzysearch-js/js/modules/IndexOfFS');
var wordCountFS = require('fuzzysearch-js/js/modules/WordCountFS');

/**
 * Constants variables
 */
const
    nameWeight = 0.3,   //used in weighted sums
    geoWeight = 0.7,    //used in weighted sums
    countries = {
        'CA':"Canada",
        'US':"USA"
    },
    provinces = {
        '01':'AB',
        '02':'BC',
        '03':'MB',
        '04':'NB',
        '05':'NL',
        '07':'NS',
        '13':'NT',
        '14':'NU',
        '08':'ON',
        '09':'PE',
        '10':'QC',
        '11':'SK',
        '12':'YT'
    };

/**
 * Formats the keys of the JSON object
 * @param key
 * @param value
 * @returns {*}
 */
function keyFormatter(obj) {
    obj.forEach(function (element) {
        element.name = element.ascii;
        delete element.ascii;

        element.latitude = element.lat;
        delete element.lat;

        element.longitude = element.long;
        delete element.long;

        delete element.objectID;

        element.score = 0;
    });
    console.log("\nFormatted:");
    //console.log(obj);
    return obj;
}

/**
 * Formats the keys of the JSON object
 * @param key
 * @param value
 * @returns {*}
 */
function cityFormatter(obj) {
    obj.forEach(function (element) {
        if (element.country==='CA') {
            element.admin1 = provinces[element.admin1];
        } else if (element.country==='US') {
            // the states of us are already in the object...
        }
        element.country = countries[element.country];
        element.name += ', '+element.admin1+', '+element.country;
        delete element.country;
        delete element.admin1;
        delete element.id;
    });
    //console.log(obj);
    return obj;
}

/**
 * Improves searches by geolocalization. Could also be done
 * in search engine but requires changes in index. For more
 * info: https://www.algolia.com/doc/javascript#geo-search
 * @param content
 */
function geoSearch(q,lat,lon,obj,g) {
    var fuzzyName = new FuzzySearch(obj, {'caseSensitive': false,'termPath': 'name','minimumScore': 300});
    fuzzyName.addModule(levenshteinFS({'maxDistanceTolerance': 3, 'factor': 3}));
    fuzzyName.addModule(indexOfFS({'minTermLength': 3, 'maxIterations': 500, 'factor': 3}));
    fuzzyName.addModule(wordCountFS({'maxWordTolerance': 3, 'factor': 1}));

    var result = fuzzyName.search(q);
    var temp = [];
    result.forEach(function (r) {
        console.log('fuzzy score');
        console.log(r.score);
        console.log(r.value);
        var nameScore = nameWeight*r.score/700;
        console.log(nameScore);
        var geoScore = geoWeight*getDistanceScore(lat, lon, r.value.latitude, r.value.longitude,g);
        console.log(geoScore);
        r.value.score = nameScore+geoScore;
        temp.push(r.value);
    });
    obj = temp;
    delete temp;

    //console.log(obj);
    return obj;
}

/**
 * Calculate a geolocation score
 * @param lat1
 * @param lon1
 * @param lat2
 * @param lon2
 * @returns {number}
 */

function getDistanceScore(lat1,lon1,lat2,lon2,g) {
    var d = getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2);
    var n = (d/g<=10)?d/g/10:1;
    return 1 - n;
}

/**
 * Calculates distance between two locations, based on latitude and longitude
 * @param lat1
 * @param lon1
 * @param lat2
 * @param lon2
 * @returns {number}
 */
function getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) {
    var R = 6371; // Radius of the earth in km
    var dLat = deg2rad(lat2-lat1);  // deg2rad below
    var dLon = deg2rad(lon2-lon1);
    var a =
            Math.sin(dLat/2) * Math.sin(dLat/2) +
            Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
            Math.sin(dLon/2) * Math.sin(dLon/2)
        ;
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    var d = R * c; // Distance in km
    return d;
}

/**
 * Degree to Radian
 * @param deg
 * @returns {number}
 */
function deg2rad(deg) {
    return deg * (Math.PI/180)
}

/**
 * Sorts suggestions by score
 * @param obj
 * @returns {*}
 */
function sortByScore(obj) {
    obj.sort(function(a, b){
        return b.score - a.score;
    });
    //console.log(obj);
    return obj;
}

/**
 *
 * @param obj
 */
module.exports = function jsonFormatter(q,lat,lon,obj,g) {
    if(obj.length>0) {
        obj = keyFormatter(obj);
        console.log("\nBetter Search:");
        obj = geoSearch(q,lat,lon,obj,g);
        console.log("\nCity Formatted:");
        obj = cityFormatter(obj);
        console.log("\nResults sorted by score:");
        obj = sortByScore(obj);
        console.log("\nStringify:");
    }
    obj = JSON.stringify({suggestions:obj},null,4);
    //console.log(temp);
    return obj;
}