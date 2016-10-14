var util = require('./util');

var SearchController = {};
SearchController.getSuggestions = function(cities, options, callback) {
  var startTime = new Date().getTime();
  console.log('SearchController.getSuggestions with query: ', options);

  var suggestions = [];
  var err= '';

  // Parse and ensure user data
  var queryString = options.q || '';
  queryString = queryString.toLowerCase().trim();
  options.latitude = parseFloat(options.latitude) || null;
  options.longitude = parseFloat(options.longitude) || null;

  //Loop on the whole array of cities.
	for (var i = 0; i < cities.length; i++) {
	    var city = cities[i];
	    if (city.population >= 5000) {
        //Get the score by string score algorithm
        var score = util.getCityNameMatchingScore(city.name.toLowerCase(), queryString);

        // if score
        if (score > 0 ) {
          suggestions.push({
            name: util.cityDisplayInfo(city),
            latitude: city.latitude,
            longitude: city.longitude,
            score: score,
          });
        }
      }
    }

  //Sort suggestion by matching score.
  suggestions = util.sortByScore(suggestions);

  // Log search time
  var endTime = new Date().getTime();
  var executionTime = endTime - startTime;
  console.log('%d cities found in %d milliseconds.', suggestions.length, executionTime);

  return callback(err, suggestions);
};

module.exports = SearchController;
