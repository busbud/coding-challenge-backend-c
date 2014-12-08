//Code samples to test
var _ = require('lodash');
var cp = require('./citiesParser.js');
var matches = require('./matches.js');
var toJSON = require('./cityToJSON.js');
var app = require('./app');

console.log(app.callbacks);
app.callbacks.done = function(err,server) {
	console.log(server);
};
// var emptyObj={someProp:undefined, otherProp:undefined};
// var undefObj;
// console.log(undefObj.propOfUndefObj); //This throws an error - JS is not totally forgiving
// if (emptyObj) {
//     console.log('Empty object is truthy'); //It's truthy
// }
// else {
//     console.log('emptyObj is falsy');
// }
// cp.getCities('./data/cities_canada-usa.tsv', function done(err, cities) {
//     if (err) {
//         return console.error(err);
//     }
//     console.log(_(cities).pluck('population').max().value()); //NYC, ~8m
//     //if (process.argv[2]) {
//     //    console.log(_(matches.getMatches(cities, process.argv[2])).map(function (c) {
//     //        return toJSON.getJSON(c, 0);
//     //    }).value()
//     //    );
//     //}
//     // console.log(cities['5881791']);
// });
//i = 0;
//parse_stream.on('data', function (chunk) {
//    if (i < 10) {
//        console.log(chunk[0]);
//        i++;
//    }
//});
////console.log(parse_stream);