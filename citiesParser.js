//Parser for cities file
var fs = require('fs');
var csv = require('csv');
var _ = require('lodash');

DESIRED_HEADERS = {//Belongs in a separate options file?
    id: 'id', primary_name: 'name', ascii_name: 'ascii', alt_names: 'alt_name', 
    lat: 'lat', long: 'long', country: 'country', state: 'admin1', population: 'population', tz: 'tz'
};

exports.getCities = function (file_to_parse, done) {//Builds a flat collection of city objects from entire file, indexed by id; passes it as cities to done(err,cities_flat)
    var output = {};
    var parser = parseFile(file_to_parse);
    var headerKey;
    var dummy;
    parser.on('data', function (chunk) {
        if (!headerKey) {
            headerKey = constructHeaderKey(chunk);//I'm not convinced this isn't a race condition
            return;
        }
        var city = parseCity(chunk, headerKey);
        output[city.id] = city;
    });
    parser.on('error', function (err) {
        done(err);
    });
    parser.on('end', function () {
        done(null, output);
    });
};

function parseFile(file_to_parse) {//Parses file_to_parse and returns Parser object, which can be read as a stream of
    var file_stream = fs.createReadStream(file_to_parse);
    var parser = csv.parse({ delimiter: '\t', quote: '', escape: '' });
    file_stream.pipe(parser);
    return parser;
}

function constructHeaderKey(headerLine) {//Returns object with indices corresponding to desired headers
    var output= {};
    for (var prop in DESIRED_HEADERS) {
        output[prop] = _.findIndex(headerLine, function (h) {
            return h === DESIRED_HEADERS[prop];
        });
    }
    return output;
}

function parseCity(cityLine, headerKey) {//Extracts city object from array of info entries;
    if (!headerKey) {
        throw new Error('No valid headerKey supplied.');
    }
    var city = {};
    for (var prop in headerKey) {
        city[prop] = cityLine[headerKey[prop]];
    }
    return convertCity(city);
}

function convertCity(city) {//Clean up selected fields
    var parsed_alt=city.alt_names.split(',');
    // if (_.random(0,100)===50) { console.log(parsed_alt);}
    city.alt_names=parsed_alt;
    //TODO: Convert state to CA province if numeric
    var numeric_fields=['id','lat','long','population'];
    for (var key in numeric_fields) {
        var field=numeric_fields[key];
        city[field]=Number(city[field]);
    }
    return city;
}