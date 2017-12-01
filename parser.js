'use strict';
var fs = require('fs');
var es = require("event-stream");

/**
 * - using counter instead of ids => ensures unicity, and is better for the array storage
 * - input sanitizing can be improved
 */
var isFirstLine = true; // We skip header 
var counter = 0;
module.exports = function (sourceFile) {

    return fs.createReadStream(sourceFile)
        .pipe(es.split("\n")) // Split Lines
        .pipe(es.mapSync(function (data) { // not sure for the need of sync here. It was in the example, I should check it (perf on denormalize is not a priority)
            if (!isFirstLine && data) {
                return createEntry(...data.split("\t"));
            }
            isFirstLine = false;
        }))
}

/** Cities creation ***************************************************************/
var createEntry = function (geonameid, name, asciiname, alternatenames, latitude, longitude, feature_class, feature_code, country_code, cc2, admin1_code, admin2_code, admin3_code, admin4_code, population, elevation, dem, timezone, modification_date) {
    // when opening TSV in excel I saw bad lines, so I added sanity checks, but it was actually a bad rendering
    if (!latitude || !longitude || isNaN(latitude) || isNaN(longitude)) {
        console.log("Invalid lat/lon detected for: " + name);
    } else if (country_code == "US" || country_code == "CA") {
        var cleanName = nameCleaner(asciiname);
        return {
            id: counter++,
            name: formatCityName(name, country_code, admin1_code),
            latlon: [latitude, longitude],

            // Note: here for quick demo. This is not a robust nor an optimal solution. Some cities are very close and seem to be duplicates => check Bay Point
            disambiguationName: formatCityName(name + " (Pop: " + population + ")", country_code, admin1_code),
            isAmbiguous: false,

            // Note that those do not have to be in the data file. they are in it just for debug purpose
            ascii: cleanName,
            altNames: alternatenames.split(",").map(nameCleaner).filter(Boolean).filter(a => a != cleanName),
        }
    }
}

var nameCleaner = (str) => str.trim().toLowerCase().replace(/"/g, '\\"')

// Mostly taken from https://raw.githubusercontent.com/barodeur/cities-suggestion-engine/06ce7d3997f6208627052f17be218014400387a0/handlers/suggestions.js
const CANADA_PROVINCES = {
    '00': '', // TODO handle proper comma formatting if this case happens
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

function formatCityName(name, countryCode, adminCode) {
    let admin = (countryCode === 'CA')
        ? CANADA_PROVINCES[adminCode]
        : adminCode;

    return `${name}, ${admin}, ${COUNTRIES[countryCode]}`;
}
