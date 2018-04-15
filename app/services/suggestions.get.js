/**
 * Get a cities suggestions for an AutoComplete list
 *
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Cache citites in a trie
 * @type {Trie}
 */
var trieCache = null;

/**
 * Build the cities cache if not already built
 * 
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function buildCache(req, res) {

  return function(callback) {

    // The cache is already built
    if (di.is.not.null(trieCache)) {
      return callback();
    }

    trieCache = new di.Trie();

    // Options: populationLowerBound
    var filteringOptions = di.config.getSuggestions('city').filtering;

    // Get the cities that has population gte
    var cities = di.city.getCities(filteringOptions.populationLowerBound);

    // Sort by population in descending order
    cities.sort(function(a, b) {
      return b.population - a.population;
    });

    // Foreach city
    cities.forEach(function(city) {

      // Normalize the city name
      var cityName = di.normalizers.name(city.name);

      // Add the city to the trie
      trieCache.add(cityName, di.deepcopy(city));

    });

    callback();

  };

}

/**
 * Get a list of suggestions 
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function getSuggestions(req, res) {

  return function(callback) {

    // Options: fuzziness, prefixLength, maxResults, extraResults
    var matchingOptions = di.config.getSuggestions('city').matching;

    // Get extra results for better scoring
    matchingOptions.maxResults += matchingOptions.extraResults;

    // Get suggestions
    var suggestions = trieCache.find(req.params.q, matchingOptions);

    callback(null, suggestions);

  };

}

/**
 * Calculate the geo distance for each suggestion
 * and find the min distance and the max distance
 * if the user's geo location is passed
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function calculateGeoDistance(req, res) {

  return function(suggestions, callback) {

    var minDistance = null;
    var maxDistance = null;

    // User's longitude and latitude are not passed
    if (!req.params.longitude || !req.params.latitude) {
      return callback(null, suggestions, minDistance, maxDistance);
    }

    // Foreach suggestion
    suggestions.forEach(function(suggestion, index) {

      // Calculate the distance
      suggestion.distance = di.geolib.getDistance(suggestion.object, req.params);

      // Set the initial values for minDistance, minDistance at the first iteration
      if (index == 0) {
        minDistance = suggestion.distance;
        maxDistance = suggestion.distance;
      }

      // New min distance
      if (minDistance > suggestion.distance) {
        minDistance = suggestion.distance;
      }

      // New max distance
      if (maxDistance < suggestion.distance) {
        maxDistance = suggestion.distance;
      }

    });

    callback(null, suggestions, minDistance, maxDistance);

  };

}

/**
 * Calculate the score for each suggestion
 *
 * - Notes
 * 
 *   - Give a full score for the not applied criterias.
 *   - If one of the criterias is set to 0, it will not be applied.
 * 
 * - Scoring criterias
 *
 *   - population
 *       - Give the full population score to the highest population city,
 *         otherwise give a partial score relatively to the highest population.
 * 
 *   - prefixUniqueness:
 *       - Give the full prefixUniqueness score if the key is
 *         a unique prefix among all other cities' keys prefixes.
 *
 *   - lengthMatching:
 *       - Give the full lengthMatching score to the exact length matching,
 *         otherwise give a partial score relatively to the length difference.
 *
 *   - editDistance:
 *       - Give the full editDistance score when editDistance = 0,
 *         otherwise give a partial score relatively to the editDistance.
 *       - It doesn't apply if the fuzziness matching option is set to 0.
 *
 *   - geoDistance
 *       - Give the full geoDistance score to the nearest city,
 *         otherwise give a partial score relatively to the distance.
 *       - It doesn't apply if the user's geo location is not passed or
 *         if we have just one suggestion.
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function calculateScores(req, res) {

  return function(suggestions, minDistance, maxDistance, callback) {

    // No suggestions
    if (!suggestions.length) {
      return callback(null, suggestions);
    }

    // Scoring weights for all criterias
    var weights = di.config.getSuggestions('city').scoring;

    // Natching options fuzziness, prefixLength, maxResults
    var matchingOptions = di.config.getSuggestions('city').matching;

    // Find the max and min population of the suggested cities
    var maxPopulation = di.lodash.maxBy(suggestions, 'object.population');
    var minPopulation = di.lodash.minBy(suggestions, 'object.population');
    maxPopulation = maxPopulation.object.population;
    minPopulation = minPopulation.object.population;

    // Foreach suggestions
    suggestions.forEach(function(suggestion) {

      // Suggestion's meta data {editDistance, siblingKeysCount, childLettersCount}
      var meta = suggestion.meta;

      // Add a score property that contains all criterias
      suggestion.score = Object.assign({}, weights);

      /**
       * Score by population
       */
      
      // Does the population criteria apply
      if (weights.population && minPopulation != maxPopulation) {

        suggestion.score.population = weights.population * 
                                      (suggestion.object.population - minPopulation) /
                                      (maxPopulation - minPopulation);

      }
      
      /**
       * Score by prefixUniqueness
       */
      
      // Does the prefixUniqueness criteria apply
      if (weights.prefixUniqueness) {

        // There are other cities that share the same prefix
        if (meta.siblingKeysCount + meta.childLettersCount != 0) {
          suggestion.score.prefixUniqueness = 0;
        }

      }

      /**
       * Score by lengthMatching
       */

      // Does the lengthMatching criteria apply
      if (weights.lengthMatching) {

        var lengthDiff = Math.abs(req.params.q.length - suggestion.key.length);
        suggestion.score.lengthMatching = weights.lengthMatching / (lengthDiff + 1);        

      }

      /**
       * Score by editDistance
       */

      // Does the editDistance criteria apply
      if (weights.editDistance && matchingOptions.fuzziness != 0) {
        suggestion.score.editDistance = weights.editDistance / (meta.editDistance + 1);
      }

      /**
       * Score by geoDistance
       */

      // Does the geoDistance criteria apply
      if (weights.geoDistance && minDistance != maxDistance &&
          req.params.longitude && req.params.latitude) {

        suggestion.score.geoDistance = weights.geoDistance * 
                                       (maxDistance - suggestion.distance) /
                                       (maxDistance - minDistance);

      }

    });

    callback(null, suggestions);

  };

}

/**
 * Normalize scores to be a value between [0, 1]
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function normalizeScores(req, res) {

  return function(suggestions, callback) {

    // Scoring weights for all criterias
    var weights = di.config.getSuggestions('city').scoring;

    // The full score
    var fullScore = di.lodash.sum(di.lodash.values(weights));

    // Foreach suggestions
    suggestions.forEach(function(suggestion) {

      suggestion.score = di.lodash.sum(di.lodash.values(suggestion.score));
      suggestion.score = di.lodash.round(suggestion.score / fullScore, 2);

    });

    callback(null, suggestions);

  };

}

/**
 * Sort the suggestions based on their scores
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function sortByScores(req, res) {

  return function(suggestions, callback) {

    // Sort by scores in descending order
    suggestions.sort(function(a, b) {
      return b.score - a.score;
    });

    callback(null, suggestions);

  };

}

/**
 * Format the ouput
 *
 * @param  {Object}   req
 * @param  {Object}   res
 * @return {Function}
 */
function output(req, res) {

  return function(suggestions, callback) {

    // Options: fuzziness, prefixLength, maxResults, extraResults
    var matchingOptions = di.config.getSuggestions('city').matching;

    // To store the final output
    var formatedSuggestions = [];

    // Remove the extra results
    suggestions.slice(0, matchingOptions.maxResults).forEach(function(suggestion) {

      formatedSuggestions.push({
        name: suggestion.object.title,
        latitude: suggestion.object.latitude,
        longitude: suggestion.object.longitude,
        score: suggestion.score
      });

    });

    // No suggestions
    if (!formatedSuggestions.length) {
      res.status(404);
    }

    callback(null, {
      suggestions: formatedSuggestions
    });

  };

}

module.exports = [

  // Build the cities cache if not already built
  buildCache,

  // Get a list of suggestions 
  getSuggestions,

  // Calculate the geo distance for each suggestion
  calculateGeoDistance,

  // Calculate the score for each suggestion
  calculateScores,

  // Normalize scores to be a value between [0, 1]
  normalizeScores,

  // Sort the suggestions based on their scores
  sortByScores,

  // Format the ouput
  output

];
