var util = require('./util');
var scorer = require('./scorer');

var SearchController = {};
SearchController.getSuggestions = function(cities, options, callback) {
  var startTime = new Date().getTime();
  var suggestions = [];
  var err= '';

  // Ensure user data
  var queryString = options.q || '';
  queryString = queryString.toLowerCase().trim();
  options.latitude = parseFloat(options.latitude) || null;
  options.longitude = parseFloat(options.longitude) || null;
  options.limit = parseInt(options.limit) || null;

  //Loop on the whole array of cities to calculate score
  cities.forEach(function(city) {
    // check only city with population
    if (city.population >= 5000) {
      var geoData  = { latitude: options.latitude, longitude: options.longitude };
      var score = scorer.getScore(city,queryString,geoData);

      // if score superior to 0 add result to suggestions
      if (score > 0 ) {
        // console.log('Score ', score);
        suggestions.push({
          name: util.formatCityName(city),
          latitude: city.latitude + '',
          longitude: city.longitude + '',
          score: +score,
        });
      }
    }
  });

  //Sort suggestion by descending score.
  suggestions = util.sortByScore(suggestions);

  // If limit is set, use it to return the max number of suggestions
  if(options.limit != undefined && options.limit < suggestions.length) {
    suggestions = suggestions.slice(0, options.limit);
  }

  // Log search time
  var endTime = new Date().getTime();
  var executionTime = endTime - startTime;
  //console.log('%d cities found in %d milliseconds.', suggestions.length, executionTime);

  return callback(err, suggestions);
};

module.exports = SearchController;
