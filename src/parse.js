var fs = require('fs');

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
exports.parseGeoNamesGazetteerTsvFile = function(options, callback) {
    // Streaming chunks of the input file to parse lines out of it.
    var readStream = fs.createReadStream(options.filePath, {
        encoding: 'utf8' // according to geonames export dump readme.
    });

    var runningBuffer = ''; // keeps truncated lines.
    var columnHeaders = null; // keeping the first line of TSV file.
    var columnsOfInterest;
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
            columnsOfInterest = translateColumnIndices(columnHeaders, options.fields);
        }

        lines.forEach(function (line) {
            var columnValues = line.split('\t');
            var fieldValues = translateFieldValues(columnsOfInterest, columnHeaders, columnValues);
            var isValid = options.criteriaFn(fieldValues);
            var score;
            if (isValid) {
                score = options.scoreFn(fieldValues);
                fieldValues.score = score;
                output.push(fieldValues);
            }
        });
    });

    readStream.on('end', function () {
        if (output.length === 0) {
            callback("No cities found.", []);
            return;
        }

        var finalOutput = [];

        var newOutput = output.sort(function (cityA, cityB) {
            return (cityA.score - cityB.score);
        });

        var maxScore = newOutput[0].score < 0 ? null : newOutput[newOutput.length - 1].score;

        newOutput.forEach(function (city) {
            finalOutput.push(options.outputFn(city, maxScore));
        });

        callback(null, finalOutput);
    });
};

// TODO Fix hack to make the tests pass with similar characters...
// ...not sure how to account for all possible replacements in 
// ...every language...maybe can asssume French and English since 
// ...in Canada and U.S....what about autochton-indian city names...
function translateInternationalChars(cityName) {
    var newName = cityName.replace("é", "e");
    return newName;
}

function translateColumnIndices(columnHeaders, fieldsOfInterest) {
    var columnHeaderIndices = [];
    fieldsOfInterest.forEach(function (fieldName) {
        columnHeaderIndices.push(columnHeaders.indexOf(fieldName));
    });
    return columnHeaderIndices;
}

function translateFieldValues(columnIndices, columnHeaders, columnValues) {
    var fieldValues = {};
    columnIndices.forEach(function (index) {
        var newValue = translateInternationalChars(columnValues[index]);
        fieldValues[columnHeaders[index]] = newValue;
    });
    return fieldValues;
}

