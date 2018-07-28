const levenshtein = require('fast-levenshtein');
const weightedMean = require('weighted-mean');

const geoScore = (city, { latitude, longitude }) => {
  if (!latitude || !longitude) return 1;

  const distance = city.distance;

  return distance === 0 ? 1 : 1 / distance;
};

const nameScore = (city, { q }) => {
  return (
    (100 -
      (levenshtein.get(city.name, q) * 100) / (city.name.length + q.length)) /
    100
  );
};

class ScoreService {
  /*
   * Register the scorers and their corresponding weights in the final score.
   */
  constructor() {
    // the name matching is 3 times more important than the distance, this can be adjusted depending on the needs
    // other scorers can also be added easily if needed
    this.scorers = [{ f: nameScore, weight: 3 }, { f: geoScore, weight: 1 }];
  }

  /*
   * This method invokes each scorer independently and returns the weighted average mean.
   */
  score(city, { q, latitude, longitude }) {
    const scores = this.scorers.map(scorer => {
      return [scorer.f(city, { q, latitude, longitude }), scorer.weight];
    });

    const meanAverage = weightedMean(scores);

    return meanAverage;
  }
}

module.exports = ScoreService;
