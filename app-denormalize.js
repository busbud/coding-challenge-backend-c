var fs = require('fs');
var through = require("through");
var tsvParser = require("./parser.js");

const sourceFile = "data/cities_canada-usa.tsv";
const dataFile = "data/data";
const mapFile = "data/map";

var mapForCities = [];
var dataForCities = [];

var lines = 0;

tsvParser(sourceFile)
  .pipe(through(function write(entry) { // use of through() instead of on("data"... => https://github.com/substack/stream-handbook
    // Not sure if more piping here would be efficient
    tokenizeNames(entry);
    dataForCities[entry.id] = entry;
    lines++;
  }, function end() {
    console.log("Loaded %s lines", lines);

    handleNameCollisions();

    // It was also possible to use JSONstream and write streams here, but I'm a bit tired after all the work...
    Promise.all([
      writeFile(dataFile + ".json", JSON.stringify(dataForCities)),
      writeFile(mapFile + ".json", JSON.stringify(mapForCities)),

      writeFile(dataFile + ".beautified.json", JSON.stringify(dataForCities, null, "    ")),
      writeFile(mapFile + ".beautified.json", JSON.stringify(mapForCities, null, "    ")),
    ]).then(function() {
      console.log("done!");
      process.exit();
    }, function() {
      console.log("failed!");
      process.exit();
    });
  }));

var tokenizeNames = function (entry) {
  splitter(entry.ascii, token => addToMap(token, entry));
  entry.altNames
    .forEach(name => splitter(name, token => addToMap(token, entry)));
}

// returns the string and all possible prefixes
// https://stackoverflow.com/questions/25788081/how-to-split-string-with-subtotal-prefix
var splitter = function (str, callback) {
  str
    .split('')
    .forEach(function (segment, index, segments) {
      callback(segments.slice(0, index + 1).join(''));
    });
}

var addToMap = function (str, entry) {
  // data is sharded by string length => reduces complexity for the search by key
  var group = mapForCities[str.length];
  if (!group) group = mapForCities[str.length] = {}; // create entry if not exist

  // data is then indexed by key
  var map = group[str]
  if (!map) map = mapForCities[str.length][str] = []; // create entry if not exist
  if (map.indexOf(entry.id) === -1) map.push(entry.id); // avoid duplicates (could also be done with an object, and, with a new step, we would take the keys)
}

var handleNameCollisions = function () {
  for (var i = 0; i < dataForCities.length; i++) {
    var entry = dataForCities[i];
    if (entry.isAmbiguous) {
      console.log("Name collision on " + entry.name);
      entry.name = entry.disambiguationName;
    }
    // not sure of the cost of delete here
    delete entry.disambiguationName;
    delete entry.isAmbiguous;
  }
}

var writeFile = function (path, data) {
  return new Promise(function(resolve, reject) {
    fs.writeFile(path, data, function(err) {
       if (err) reject(err);
       else resolve(data);
    });
  })
}