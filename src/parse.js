var fs = require('fs');
var path = require('path');

var populationThreshold = 5000;

exports.parse = function (callback) {
    var inputFilePath = path.resolve(__dirname, '../data/cities_canada-usa.tsv');
    var readStream = fs.createReadStream(inputFilePath, {
        encoding: 'utf8'
    });

    var runningBuffer = '';
    var columnHeaders = null;
    readStream.on('data', function(chunk) {
        var newData = chunk.toString('utf8');
        
        var endsWithNewLine = newData.lastIndexOf('\n') === newData.length;
        var lines = newData.split('\n');
        lines[0] = runningBuffer + lines[0];
        if (!endsWithNewLine) {
            runningBuffer = lines.splice(lines.length - 1, 1)[0];
        }

        if (!columnHeaders) {
            columnHeaders = lines.shift().split('\t');
        }
        lines.forEach(function (line) {
            var columnValues = line.split('\t');
            var populationIndex = columnHeaders.indexOf('population');
            var population = columnValues[populationIndex] - 0;
            if (population >= 5000) console.log('city', columnValues[1], 'population',
            columnValues[populationIndex]);
        });

    });
    readStream.on('end', callback);
};

