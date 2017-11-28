var es = require("event-stream");
var sort = require('sort-stream'); // Nice to have: check perf on this one

// Basic synchronous load.
// Not sure if load time is a concern, but if it is, many solutions are available...
var map = require('./data/map');
var data = require('./data/data');

module.exports = function (query, latlon) {
  var found = map[query.length][query];
  if (found)
    return es.readArray(found)
      .pipe(es.mapSync(function (index) { // TODO check if Sync is removable (create a bug for now)
        var item = data[index];
        return [item, getScore(query, item, latlon())]; // scoring is poorly implemented
      }))
      .pipe(sort(cmp))
      // Copied from: https://johnresig.com/blog/node-js-stream-playground/
      // Convert Array to strings
      .pipe(es.mapSync(formatProperty)) // Sync or we may loose order
      // Join Strings
      .pipe(es.join(","))
      // Concat Strings
      .pipe(es.wait())
      // Wrap Strings
      .pipe(es.mapSync((str) => '{"suggestions": [' + str + ']}'))
}

// sorting from : https://stackoverflow.com/questions/34956869/sorting-a-data-stream-before-writing-to-file-in-nodejs
// changed -1 & +1 to make it descending
var cmp = (x, y) => x[1] < y[1] ? 1 : x[1] == y[1] ? 0 : -1;

var formatProperty = function (item) {
  return '{"name": "' + item[0].name + '","latitude": "' + item[0].latlon[0] + '","longitude": "' + item[0].latlon[1] + '","score": ' + item[1].toFixed(1) + '}';
}

var formatPropertyDebug = function (item) {
  return '{"ascii": "' + item[0].ascii + '", "alt":["' + item[0].altNames.join('","') + '"],' + formatProperty(item).substring(1)
}

// WARNING ! score computation is poorly computed and is inspired by these ancient fellows advices:
// - for distance: http://i0.kym-cdn.com/photos/images/facebook/000/234/146/bf8.jpg
// - for names: http://i0.kym-cdn.com/photos/images/facebook/000/234/765/b7e.jpg
// even listening those songs did not help: https://www.youtube.com/watch?v=MTQnYyyAFCo and https://www.youtube.com/watch?v=W0wPNow3ymc

// Only optimization here is that score is local to the current result and not relative to other results. Relative scoring would be more costly.
var getScore = function (str, item, latlon) {
  var score = str.length / item.ascii.length; // Note: alternative names will give bad scores
  if (latlon)
    score *= getDistanceScore(latlon, item.latlon);
  return score;
}

// from: http://jonisalonen.com/2014/computing-distance-between-coordinates-can-be-simple-and-fast/
// Note: this is an approximation, so it will not be right for 'far from user' and 'close to each other' places, but, this is just for scoring, so the error seems tolerable
var getDistance = function (latlon0, latlon1) {
  var x = latlon0[0] - latlon1[0]
  var y = (latlon0[1] - latlon1[1]) * Math.cos(latlon1[0])
  return Math.sqrt(x * x + y * y);
}

var getDistanceScore = function(latlon0, latlon1) {
  var dist = getDistance(latlon0, latlon1);
  if (dist < 1) return 1; // not far
  if (dist < 3) return 0.8; // far
  if (dist < 7) return 0.5; // further
  if (dist < 10) return 0.3; // even further
  else return 0.1; // in a galaxy, far far away
}