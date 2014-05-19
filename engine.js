var csv        = require('fast-csv');
var Bloodhound = require('typeahead.js/dist/bloodhound.js');
var log        = require('debug')('engine');

var canadianCodeMapping = {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '13': 'NT',
    '07': 'NS',
    '14': 'NU,',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'YT',
    '05': 'NL'
};

var engine;

function initialize(callback) {
    var dataFile = process.env.DATAFILE || (__dirname + '/data/cities_canada-usa.tsv');
    _readDataFromTSVFile(dataFile, function(error, response) {
        engine       = new Bloodhound({
            local         : response,
            datumTokenizer: function(datum) {
                return Bloodhound.tokenizers.whitespace(datum.name + ' ' + datum.alt_name.split(',').join(' '));
            },
            queryTokenizer: Bloodhound.tokenizers.whitespace
        });
        engine.initialize();

        response = 'Started suggestion engine with ' + dataFile;
        callback(error, response);
    });

}

function search(query, longitude, latitude, callback) {
    engine.get(query, function(results) {
        var response;
        if ('undefined' == typeof longitude || 'undefined' == typeof latitude) {
            response = results.map(function(result) {
                return {
                    id       : result.id,
                    name     : result.label,
                    longitude: result.long,
                    latitude : result.lat,
                    score    : 1
                };
            });

        } else {
            response = results.map(function(result) {
                var euclideanDistance = 0;
                var squareRootEuclideanDistance = Math.pow(longitude - result.long, 2) + Math.pow(latitude - result.lat, 2);
                if (squareRootEuclideanDistance > 0) {
                    euclideanDistance = 1 / squareRootEuclideanDistance;
                }

                return {
                    id       : result.id,
                    name     : result.label,
                    longitude: result.long,
                    latitude : result.lat,
                    score    : euclideanDistance
                };

            });

            var scores = response.map(function(x) {
              return x.score;
            });

            var max = Math.max.apply(null, scores);

            response.forEach(function(x) {
                x.score = (x.score / max);
            });

            response.sort(function(a, b) {
                return b.score - a.score;
            });

        }

        callback(null, response);
    });

}

function _readDataFromTSVFile(filename, callback) {
    var response = [];
    csv.fromPath(filename, {
        delimiter  : '\t',
        headers    : true,
        ignoreEmpty: true,
        trim       : true,
        escape     : '',
        quote      : ''
    })
    .transform(function(record) {
        if ('CA' == record.country) {
            record.admin1 = canadianCodeMapping[record.admin1];
        }

        return {
            id      : record.id,
            name    : record.name,
            alt_name: record.alt_name,
            label   : [record.name, record.admin1, record.country].join(', '),
            long    : record.long,
            lat     : record.lat
        };
    })
    .on('record', function(record) {
      response.push(record);
    })
    .on('end', function() {
        callback(null, response);
    })
    .on('error', function(error) {
        log('Error parsing record: ' + error);
    });
}

module.exports.initialize = initialize;
module.exports.search     = search;