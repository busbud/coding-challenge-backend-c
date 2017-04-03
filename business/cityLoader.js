var fs = require("fs");
var es = require("event-stream");

var defaultFile = "data/cities_canada-usa.tsv";

var divisionsOfCanada = [
    null,
    "AB", // Alberta
    "BC", // British Colombia
    "MB", // Manitoba
    "NB", // New Brunswick
    "NL", // Newfoundland and Labrador
    null,
    "NS", // Nova Scotia
    "ON", // Ontario
    "PE", // Prince Edward Island
    "QC", // Québec
    "SK", // Saskatchewan
    "YT", // Yukon
    "NT", // Northwest Territories
    "NU", // Nunavut
];

/**
 * Loads cities from file and write them to the provided writeable stream
 * @param {Stream} res
 * @param {string*} file - a file containing the cities to load
 */
var loadAndWriteTo = function(res, file) {
    var cityFile = file || defaultFile;
    // Read cities from data file
    return fs.createReadStream(cityFile)
        // Split Strings
        .pipe(es.split("\n"))
        // Split Strings into Array
        .pipe(es.map(function(data, cb) {
            if (data[0] === "i")
                cb(); // drop the header line
            else
                cb(null, data.split("\t"));
        }))
        // Convert array to objects containing expected output data
        .pipe(es.map(function(data, cb) {
            var city = getCityObjectFrom(data);
            if (!city)
                cb(); // drop this city
            else
                cb(null, city);
        }))
        .pipe(es.stringify())
        // Join Strings
        .pipe(es.join(","))
        // Concat Strings
        .pipe(es.wait())
        // format result
        .pipe(es.mapSync(function(cities) {            
            return '{ "suggestions": [' + cities + '] }';
        }))
        .pipe(res);
};

/**
 * Loads cities from file and write them to the provided writeable stream
 * @param {Stream} res - a result stream
 * @param {string} searchTerm - a search term to filter on
 */
var loadAndFilterTo = function(res, searchTerm) {
    var cityFile = file || defaultFile;
    // Read cities from data file
    return fs.createReadStream(cityFile)
        // Split Strings
        .pipe(es.split("\n"))
        // Split Strings into Array
        .pipe(es.map(function(data, cb) {
            if (data[0] === "i")
                cb(); // drop the header line
            else
                cb(null, data.split("\t"));
        }))
        // Convert array to objects containing expected output data
        .pipe(es.map(function(data, cb) {
            var city = getCityObjectFrom(data);
            if (!city)
                cb(); // drop this city
            else
                cb(null, city);
        }))
        .pipe(es.stringify())        
        // Join Strings
        .pipe(es.join(","))
        // Concat Strings
        .pipe(es.wait())
        // format result
        .pipe(es.mapSync(function(cities) {            
            return '{ "suggestions": [' + cities + '] }';
        }))
        .pipe(res);
};

/**
 * Loads cities from file and store them to the provided city index
 * @param {Object} cityIndex
 * @param {string*} a file containing the cities to load
 */
function loadAndStoreTo(cityIndex, file) {
    var cityFile = file || defaultFile;
    
    // Read cities from data file
    var readStream = fs.createReadStream(cityFile)
        // Split Strings
        .pipe(es.split("\n"))
        // Convert strings into arrays
        .pipe(es.map(function(data, cb) {
            if (data[0] === "i")
                cb(); // drop the header line
            else
                cb(null, data.split("\t"));
        }))
        // Convert arrays to city objects
        .pipe(es.map(function(data, cb) {
            var city = getCityObjectFrom(data);
            if (!city)
                cb(); // drop this city
            else
                cb(null, city);
        }))
        // Add cities to index
        .pipe(es.map(function(city, cb) {            
            cityIndex.addCity(city);
            cb();
        }));
       
        return readStream;
}

/**
 * Convert an array to city object
 * @param {array} data 
 */
function getCityObjectFrom(data) {
    // Warning : following lines must be uncommented as soon as we have cities with a population greater than 5,000 people
    // var population = parseInt(data[14]);
    // if (population && population < 5000) {
    //     return null;
    // }
    
    var state = data[10];
    var country = data[8];
    
    if (country === "CA") {
        state = divisionsOfCanada[parseInt(state)];
    }
    return {
        name: data[1],
        state: state,
        country: country,
        latitude: data[4],
        longitude: data[5]
    };
}

module.exports.loadAndWriteTo = loadAndWriteTo;
module.exports.loadAndStoreTo = loadAndStoreTo;