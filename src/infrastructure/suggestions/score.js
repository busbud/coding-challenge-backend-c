const calculateScore = (minScore, maxScore, score) => {
  if (minScore === maxScore) {
    return minScore;
  }

  return (minScore - score) / (minScore - maxScore);
};

// eslint-disable-next-line no-underscore-dangle
const minScore = (suggestions) => Math.min(...suggestions.map((suggestion) => suggestion._score));

module.exports.calculate = calculateScore;
module.exports.minScore = minScore;
