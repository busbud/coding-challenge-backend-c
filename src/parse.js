var fs = require('fs');
var path = require('path');

var Suggestions = require("./model/Suggestions.js");

// GeoNames Gazetteer constants.
var GEONAMES_SOURCE_FILE_PATH = path.resolve(__dirname, '../data/cities_canada-usa.tsv');
var GEONAMES_FIELDS_ARRAY = [ "population", "name", "country", "lat", "long" ];

/**
 * Parses a GeoNames Gazetteer extract TSV file using SuggestedCity model.
 * 
 * @param {Object} options
 * @param {string} options.filePath
 * @param {Function} options.criteriaFn
 * @param {string[]} options.criteriaFn.columnHeaders
 * @param {string[]} options.criteriaFn.columnValues
 * @param {Function} options.outputFn
 * @param {string[]} options.outputFn.columnHeaders
 * @param {string[]} options.outputFn.columnValues
 * @param {Function} callback
 * @param {string|null} callback.err
 * @param {Array.<Object>} callback.output
 */
exports.parseGeoNamesGazetteerTsvFile = function(queryOptions, callback) {
    // Streaming chunks of the input file to parse lines out of it.
    var readStream = fs.createReadStream(GEONAMES_SOURCE_FILE_PATH, {
        encoding: 'utf8' // according to geonames export dump readme.
    });

    var runningBuffer = ''; // keeps truncated lines.
    var columnHeaders = null; // keeping the header (first) line of TSV file.
    var columnIndices;
    var suggestions = new Suggestions();
    readStream.on('data', function(chunk) {
        var newData = chunk.toString('utf8');

        if (newData.indexOf('\n') === -1) {
            // No (complete) lines to parse. Wait for next chunk...
            runningBuffer += newData;
            return;
        }

        var lines = newData.split('\n');

        lines[0] = runningBuffer + lines[0]; // Append last truncated line.
        
        var endsWithNewLine = (newData.lastIndexOf('\n') === newData.length);
        if (!endsWithNewLine) {
            // Keep truncated line for next chunk.
            runningBuffer = lines.splice(lines.length - 1, 1)[0];
        }

        if (lines.length === 0) {
            return; // No (complete) lines to parse. Wait for next chunk...
        }

        if (!columnHeaders) {
            // Record header (first) line of TSV file.
            columnHeaders = lines.shift().split('\t');
            columnIndices = getFieldIndices(columnHeaders, GEONAMES_FIELDS_ARRAY); // for matching 
                // column values later.
        }

        lines.forEach(function (line) {
            var columnValues = line.split('\t');
            var suggOptions = translateFieldValues(columnIndices, columnHeaders, columnValues);
            suggestions.add(suggOptions, queryOptions);
        });
    });

    readStream.on('end', function () {
        if (suggestions.isEmpty()) {
            callback("No cities found.", []);
            return;
        }

        var suggestionsResult = suggestions.get();
        callback(null, suggestionsResult);
    });
};

/**
 * @param {string[]} columnHeaders
 * @param {string[]} fieldsOfInterest
 * @return {number[]}
 */
function getFieldIndices(columnHeaders, fieldsOfInterest) {
    var columnHeaderIndices = [];
    fieldsOfInterest.forEach(function (fieldName) {
        columnHeaderIndices.push(columnHeaders.indexOf(fieldName));
    });
    return columnHeaderIndices;
}

/**
 * @param {number[]} columnIndices
 * @param {string[]} columnHeaders
 * @param {string[]} columnValues
 * @return {SuggestedCity}
 */
function translateFieldValues(columnIndices, columnHeaders, columnValues) {
    var fieldValues = {};
    columnIndices.forEach(function (index) {
        var newValue = columnValues[index];
        var newKey = columnHeaders[index];

        fieldValues[newKey] = newValue;
    });
    return fieldValues;
}

