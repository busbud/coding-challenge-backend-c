// Adapted from https://github.com/IndigoUnited/js-weighted-mean/blob/master/index.js
module.exports = function weightedMean(weightedValues) {
  const totalWeight = weightedValues.reduce((sum, weightedValue) =>  sum + weightedValue[1], 0);

  return weightedValues.reduce((mean, weightedValue) => mean + weightedValue[0] * weightedValue[1] / totalWeight, 0);
};
