const score = require('./scores');
const config = require('../config.json');

module.exports = function suggest(data, term, coords) {

  const regex = new RegExp('^' + term.split('').join('.*?'), 'i');

  var maxLev = maxProx = maxPop = 0;
  var minLev = minProximity = minPopulation = Infinity;

  const suggestions = data
    .filter((location) => {
      return regex.test(location.canonical) ? location : false;
    })
    .map((loc) => {

      loc.levenshtein = score.levenshtein(term, loc.canonical);
      maxLev = Math.max(loc.levenshtein, maxLev);
      minLev = Math.min(loc.levenshtein, minLev);

      maxPop = Math.max(loc.population, maxPop);
      minPopulation = Math.min(loc.population, minPopulation);

      if (coords && coords.lat && coords.lng) {
        loc.proximity = score.proximity(coords, loc);
        maxProx = Math.max(loc.proximity, maxProx);
        minProximity = Math.min(loc.proximity, minProximity);
      } else {
        loc.proximity = 0;
      }

      return loc;
    })
    .map((loc) => {

      var popScore = (loc.population - minPopulation) /
        (maxPop - minPopulation);
      var levScore = (maxLev - loc.levenshtein) /
        (maxLev - minLev);
      var proxScore = (maxProx - loc.proximity) /
        (maxProx - minProximity);

      loc.score = (popScore * config.weights.population) +
        (levScore * config.weights.levenshtein) +
        (proxScore * config.weights.proximity);

      return loc;
    })
    .sort((a, b) => {
      if (a.score >= b.score) {
        return -1;
      } else {
        return 1;
      }
    })
    .slice(0, config.results)
    .map((loc) => {
      return {
        name: loc.city + ' ' + loc.state_province + ', ' + loc.country,
        city: loc.city,
        state_province: loc.state_province,
        country: loc.country,
        latitude: loc.lat,
        longitude: loc.lng,
        score: loc.score
      };
    });

  return suggestions;
};