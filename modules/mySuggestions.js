const config = require('./global.js');
/**
 * Given a location name and an array of suggested destinations, it ranks the destinations by name
 * the less number of charcters the name and the name of the suggestion will be top ranked scored
 * also, given a weight (by default 1 representing 100%), this score is weighted
 * @param [String] string
 * @returns []
 */
let getRankingByName = (originCityName, arrayDestination, weight = 1) => {
  let arrayDeltaNames = arrayDestination.map(
    (destination) =>
      Math.abs(destination.name.length - originCityName.length) + 1
  );

  const minDifference = Math.min(...arrayDeltaNames);

  let arrayRatiosDeltaNames = arrayDeltaNames.map((d) => d / minDifference);
  //Normalizing relatives distances
  let arrayNormalizedDetlaNames = normalizeArray(arrayRatiosDeltaNames);
  //inverting scale 1-normalized
  arrayScored = invertedScoreArray(arrayNormalizedDetlaNames);

  return arrayScored.map((score, i) => score * weight);
};

/**
 * This function, calculates a rate between each distance and the minimal distance
 * this ratio is normalized
 * the normalized ratio is weighted by the weight of importance of the distance
 * array of objects 'rankedDestination'
 * @param [{}]
 *
 * * array of objects 'rankedDestination'
 * @returns [{}]
 */
let getRankingByDistance = (arrayDestination, weight = 1) => {
  let arrayDistance = arrayDestination.map((d) => d.distance);
  const minDistance = Math.min(...arrayDistance) + 1;
  let arrayRatioDistance = arrayDistance.map((d) => d / minDistance);
  //Normalizing relatives distances
  let arrayNormalizedDistances = normalizeArray(arrayRatioDistance);
  //inverting scale 1-normalized
  arrayScored = invertedScoreArray(arrayNormalizedDistances);

  return arrayScored.map((score, i) => score * weight);
};

function normalizeArray(arrayTmp) {
  const totalSum = arrayTmp.reduce(
    (accumulator, currentValue) => accumulator + currentValue
  );
  return arrayTmp.map((e) => e / totalSum);
}

function invertedScoreArray(arrayTmp) {
  return arrayTmp.map((e) => 1 - e);
}

/**
 * This method calculates the total score (ranking) of the destinations, according to distance and name, both totalized scores
 * if there is no distance then only the name score is returned
 * @param [String] string
 *
 * @returns []
 */
module.exports.getRanking = (
  cityName,
  arrayDestination,
  weightName,
  weightDistance
) => {
  let arrayScoreRankedByName = getRankingByName(
    cityName,
    arrayDestination,
    weightName
  );
  if (weightDistance != 0) {
    let arrayScoreRankedByDistance = getRankingByDistance(
      arrayDestination,
      weightDistance
    );
    arrayDestination.forEach((e, i) => {
      e.score = arrayScoreRankedByName[i] + arrayScoreRankedByDistance[i];
      return e;
    });
  } else {
    arrayDestination.forEach((e, i) => {
      e.score = arrayScoreRankedByName[i];
      return e;
    });
  }
  arrayDestination.sort((a, b) => b.score - a.score); //descendent order

  return arrayDestination;
};
