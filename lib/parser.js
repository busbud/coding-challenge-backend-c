'use strict';
var fs = require('fs');
var es = require('event-stream');

/** Cities creation ***************************************************************/

/**
 * Function to normalize the names, especially the alternate names
 * TODO this is a basic normalization, it can be improved
 * 
 * @param {string} str The name to be normalized
 * @returns {string} The normalized name
 */
var nameCleaner = str => str
    .trim()
    .toLowerCase()
    .replace(/"/g, '\\"')

/**
 * Creates a city object from the fields found on one line of the data file
 * 
 * String params are the columns of the source file, specified in the data/README.md
 * 
 * @param {string} geonameid 
 * @param {string} name
 * @param {string} asciiname
 * @param {string} alternatenames
 * @param {string} latitude
 * @param {string} longitude
 * @param {string} feature_class
 * @param {string} feature_code
 * @param {string} country_code
 * @param {string} cc2
 * @param {string} admin1_code
 * @param {string} admin2_code
 * @param {string} admin3_code
 * @param {string} admin4_code
 * @param {string} population
 * @param {string} elevation
 * @param {string} dem
 * @param {string} timezone
 * @param {string} modification_date
 * @returns {City} Object representing the city
 */
var createEntry = function (geonameid, name, asciiname, alternatenames, latitude, longitude, feature_class, feature_code, country_code, cc2, admin1_code, admin2_code, admin3_code, admin4_code, population, elevation, dem, timezone, modification_date) {
    // When opening TSV in excel I saw bad lines, so I added sanity checks, but it was actually a bad rendering
    if (!latitude || !longitude || isNaN(latitude) || isNaN(longitude)) {
        console.log("Invalid lat/lon detected for: " + name);
    } else if (country_code === "US" || country_code === "CA") {
        var cleanName = nameCleaner(asciiname);
        
        return {
            name: formatCityName(name, country_code, admin1_code),
            latlon: [latitude, longitude],

            // Note: here for quick demo. This is not a robust nor an optimal solution. Some cities are very close and seem to be duplicates => check Bay Point in the provided TSV file
            disambiguationName: formatCityName(name + " (Pop: " + population + ")", country_code, admin1_code),
            isAmbiguous: false,

            // Note that those do not have to be in the data file. they are in it just for debug purpose
            ascii: cleanName,
            altNames: alternatenames.split(",")
                .map(nameCleaner)
                .filter(Boolean)
                .filter(a => a !== cleanName),
        }
    }
}

/**
 * Parse the source data file into lines and then into objects
 * - using counter instead of ids => ensures unicity, and is better for the array storage
 * - input sanitizing can be improved
 * 
 * @param {string} sourceFile Path to the file to be parsed
 * @returns {Stream} The stream of objects
 */
module.exports = function (sourceFile) {
    var isFirstLine = true; // We skip header 
    
    return fs.createReadStream(sourceFile)
        .pipe(es.split("\n")) // Split Lines
        .pipe(es.mapSync(function (data) { // Not sure for the need of sync here. It was in the example, I should check it (perf on denormalize is not a priority)
            if (!isFirstLine && data) {
                return createEntry(...data.split("\t"));
            }
            isFirstLine = false;
        }))
}

/**
 * Conversion map from admin codes to String literals
 * Mostly taken from https://raw.githubusercontent.com/barodeur/cities-suggestion-engine/06ce7d3997f6208627052f17be218014400387a0/handlers/suggestions.js
 */
const CANADA_PROVINCES = {
    '00': '', // TODO handle proper comma formatting if this case happens (it doesn't for now)
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '05': 'NL',
    '07': 'NS',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'YT',
    '13': 'NT',
    '14': 'NU',
};

const COUNTRIES = {
    CA: 'Canada',
    US: 'USA',
};

/**
 * 
 * @param {string} name Original name of the city
 * @param {string} countryCode Country code (2 letters)
 * @param {string} adminCode Admin code to get the province (2 digits for Canada)
 * @returns Formatted Name for the city
 */
function formatCityName(name, countryCode, adminCode) {
    const admin = (countryCode === 'CA')
        ? CANADA_PROVINCES[adminCode]
        : adminCode;

    return `${name}, ${admin}, ${COUNTRIES[countryCode]}`;
}
