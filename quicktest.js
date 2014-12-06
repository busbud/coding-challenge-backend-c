//Code samples to test
var cp = require('./citiesParser.js');
var matches = require('./matches.js');

cp.getCities('./data/cities_canada-usa.tsv', function done(err, cities) {
    if (err) {
        return console.error(err);
    }
    if (process.argv[2]) {
        console.log(matches.getMatches(cities, process.argv[2]));
    }
    //console.log(cities['5881791']);
});
//i = 0;
//parse_stream.on('data', function (chunk) {
//    if (i < 10) {
//        console.log(chunk[0]);
//        i++;
//    }
//});
////console.log(parse_stream);