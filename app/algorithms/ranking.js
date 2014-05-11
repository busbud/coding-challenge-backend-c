
/********************
 * Import Modules
 *******************/

var Classy = require(__dirname + "/../structures/classy.js");





/********************
 * Basic Rank Algorithm
 *******************/

// Basic Ranking Exponential Distribution
// Make it so large cities have a exponentially larger ranking than smaller
var BREDLambda = 1.5,
  BREDHighPopulation = 5000000,
  BREDLowPopulation = 5000,
  BREDHighX = 0.3,
  BREDLowX = 2;

function ComputeRankPopulation(population) {
  // First create linear scale from 0 to 1 such that 0 is lowpopulation and 1 is high
  var _x = (population - BREDLowPopulation) / (BREDHighPopulation - BREDLowPopulation);
  // So software, much wow, future proofing
  if (_x < 0) {
    _x = 0;
  }
  if (_x > 1) {
    _x = 1;
  }
  // Convert this linear scale to 2 for low and 0.3 for high
  _x = ((_x * -1) + 1) * (BREDLowX - BREDHighX) + BREDHighX;

  // Return ranking for population
  return BREDLambda * Math.pow(Math.E, -1 * BREDLambda * _x);
}


// Static algorithm to score each city depending on firsthand information
// Compute statistical information about cities
// I initially thought it would look like a Normal distribution, but no ...
// Looks more like an exponential distribution
// Score for 1st Quantille 0.05, 2nd 0.1, 3rd 0.2, 4th 0.4, 5th 0.95, or so I guess
// These statistics are not useful runtime, but they were useful for creating the rank
function BasicRanking(dataList) {
  // Return object
  var _result = {};

  // City Size statistics
  // To compute median precisely, sort and pick middle, instead of "median of medians"
  var _dataListOrdSize = [];
  var _statistics = {};
  _statistics.averageCitySize = 0.0;

  // Create array with references to JSON objects to sort in place, size is not that big
  for (var c = 0; c < dataList.length; c++) {
    _dataListOrdSize.push(dataList[c]);
    _statistics.averageCitySize += (dataList[c].population / dataList.length);
  }

  // I believe V8 implementation is a mergesort for this
  // In any way, even if it is slow, it is only once when server starts
  _dataListOrdSize.sort(function (a, b) {
    return (b.population - a.population);
  });

  _result.dataListOrdSize = _dataListOrdSize;

  // Statistics
  _statistics.medianCitySize = _dataListOrdSize[Math.floor(_dataListOrdSize.length / 2)].population;
  _statistics.quintilleOneSize = _dataListOrdSize[Math.floor(_dataListOrdSize.length / 5)].population;
  _statistics.quintilleTwoSize = _dataListOrdSize[Math.floor(2 * _dataListOrdSize.length / 5)].population;
  _statistics.quintilleThreeSize = _dataListOrdSize[Math.floor(3 * _dataListOrdSize.length / 5)].population;
  _statistics.quintilleFourSize = _dataListOrdSize[Math.floor(4 * _dataListOrdSize.length / 5)].population;
  _statistics.averageCitySize = Math.floor(_statistics.averageCitySize);
  _statistics.smallestCitySize = _dataListOrdSize[0].population;
  _statistics.biggestCitySize = _dataListOrdSize[_dataListOrdSize.length - 1].population;

  _result.statistics = _statistics;

  // Compute basic ranking for all cities
  for (var r = 0; r < dataList.length; r++) {
    // Compute and add rank
    dataList[r].rank = {};
    dataList[r].rank.basic = ComputeRankPopulation(dataList[r].population);
  }

  return _result;
}




/********************
 * Ranking Static Class
 *******************/

// Class for the ranking of a city depending of its properties
var Ranking = Classy.extend({}, {
  BasicRanking: BasicRanking
});





/********************
 * Export Modules
 *******************/

module.exports = Ranking;