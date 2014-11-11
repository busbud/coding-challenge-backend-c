var fs = require('fs');
var path = require('path');

var populationThreshold = 5000;

exports.parse = function (callback) {
    var options = {
        filePath: path.resolve(__dirname, '../data/cities_canada-usa.tsv'),
        criteriaFn: function (columnHeaders, columnValues) {
            var populationIndex = columnHeaders.indexOf('population');
            var population = columnValues[populationIndex] - 0;
            return population >= 5000;
        },
        outputFn: function (columnHeaders, vals) {
            var populationIndex = columnHeaders.indexOf('population');
            var cityNameIndex = columnHeaders.indexOf('name');
            var countryCode = columnHeaders.indexOf('country');
            var latCoord = columnHeaders.indexOf('lat');
            var longCoord = columnHeaders.indexOf('long');
            var score = 1.0;
            return {
                name : vals[cityNameIndex],
                population : vals[populationIndex],
                country : vals[countryCode],
                latCoord : vals[latCoord],
                longCoord : vals[longCoord],
                score : score
            };
        }
    };
    parseGeoNamesGazetteerTsvFile(options, callback);
};

/**
 * Parses a GeoNames Gazetteer extract TSV file using a criteria
 * function to determine entries of interest and an output function to
 * translate a TSV line into the wanted output.
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
function parseGeoNamesGazetteerTsvFile(options, callback) {
    // Streaming chunks of the input file to parse lines out of it.
    var readStream = fs.createReadStream(options.filePath, {
        encoding: 'utf8' // according to geonames export dump readme.
    });

    var runningBuffer = ''; // keeps truncated lines.
    var columnHeaders = null; // keeping the first line of TSV file.
    var output = [];
    readStream.on('data', function(chunk) {
        var newData = chunk.toString('utf8');

        if (newData.indexOf('\n') === -1) {
            // No (complete) lines to parse. Wait for next chunk...
            runningBuffer += newData;
            return;
        }

        var lines = newData.split('\n');

        lines[0] = runningBuffer + lines[0]; // Append last truncated
            // line.
        
        var endsWithNewLine = (newData.lastIndexOf('\n') === newData.length);
        if (!endsWithNewLine) {
            // Keep truncated line for next chunk.
            runningBuffer = lines.splice(lines.length - 1, 1)[0];
        }

        if (lines.length === 0) {
            return; // No (complete) lines to parse. Wait for next
                // chunk...
        }

        if (!columnHeaders) {
            // Record first line of TSV file.
            columnHeaders = lines.shift().split('\t');
        }

        lines.forEach(function (line) {
            var columnValues = line.split('\t');
            var isOfInterest = options.criteriaFn(columnHeaders,
                columnValues);
            var outputEntry;
            if (isOfInterest) {
                outputEntry = options.outputFn(columnHeaders, columnValues);
                output.push(outputEntry);
            }
        });
    });

    readStream.on('end', function () {
        callback(output);
    });
};

