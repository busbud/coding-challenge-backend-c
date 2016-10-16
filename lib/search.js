var util = require('./util');

var SearchController = {};
SearchController.getSuggestions = function(cities, options, callback) {
  var startTime = new Date().getTime();
  //console.log('SearchController.getSuggestions with query: ', options);

  var suggestions = [];
  var err= '';

  // Ensure user data
  var queryString = options.q || '';
  queryString = queryString.toLowerCase().trim();
  options.latitude = parseFloat(options.latitude) || null;
  options.longitude = parseFloat(options.longitude) || null;

  //Loop on the whole array of cities to cal
  cities.forEach(function(city) {
    // check only city with population
    if (city.population >= 5000) {
      var geoData  = { latitude: options.latitude, longitude: options.longitude };
      var score = util.getScore(city,queryString,geoData);

      // if score superior to 0 add result to suggestions
      if (score > 0 ) {
        // console.log('Score ', score);
        suggestions.push({
          name: util.formatCityName(city),
          latitude: city.latitude,
          longitude: city.longitude,
          score: score,
        });
      }
    }
  });

  //Sort suggestion by descending score.
  suggestions = util.sortByScore(suggestions);

  // Log search time
  var endTime = new Date().getTime();
  var executionTime = endTime - startTime;
  //console.log('%d cities found in %d milliseconds.', suggestions.length, executionTime);

  return callback(err, suggestions);
};

module.exports = SearchController;
