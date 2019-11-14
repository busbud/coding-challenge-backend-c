const levenshtein = require("fast-levenshtein");
const haversine = require("haversine-distance");
const _ = require("lodash");

const MAX_HAVERSINE = haversine({lat:0, lon:0}, {lat:0, lon:180})

const makeScorer = ({ distWeight, nameWeight, popWeight, maxPop }, { q, latitude: latQ, longitude: longQ }) => (
  ({ name, population, latitude: latR, longitude: longR }) => {
    // lat/long distance
    let distanceScore = 0;
    if (latQ && longQ) {
      const distance = haversine({lat: latQ, lon: longQ}, {lat: latR, lon: longR});
      distanceScore = 1 - (distance / MAX_HAVERSINE);
    }

    // normalized edit distance
    const normLev = (levenshtein.get(q.toLowerCase(), name.toLowerCase()) / Math.max(q.length, name.length));
    const nameScore = 1 - normLev;

    // population
    const populationScore = population / maxPop;
    const popScoreFixed = -1/(7*populationScore + 1) + 1; // adjust for few very large cities

    const score = (
      distWeight * distanceScore +
      nameWeight * nameScore +
      popWeight  * popScoreFixed
    );

    return score;
  }
);

module.exports = { makeScorer };
