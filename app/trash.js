


function ComputeBasicRankingSorted(dataList) {
  // Local variables
  var _ordRankDataList = [];

  // Compute basic ranking for all cities
  for (var r = 0; r < dataList.length; r++) {
    // Compute and add rank
    dataList[r].rank = {};
    dataList[r].rank.basic = ComputeRankPopulation(dataList[r].population);

    _ordRankDataList.push(dataList[r]);
  }

  // Sort by basic rank
  _ordRankDataList.sort(function (a, b) {
    return (b.rank.basic - a.rank.basic);
  });

  // Return ordered list by Rank
  return _ordRankDataList;
}


function ComputeStatistics(dataList) {
  // City Size statistics
  // To compute median precisely, sort and pick middle, instead of "median of medians"
  var _ordSizeDataList = [];
  var _data = {};
  _data.averageCitySize = 0.0;

  // Create array with references to JSON objects to sort in place, size is not that big
  for (var c = 0; c < dataList.length; c++) {
    _ordSizeDataList.push(dataList[c]);
    _data.averageCitySize += (dataList[c].population / dataList.length);
  }

  // I believe V8 implementation is a mergesort for this
  // In any way, even if it is slow, it is only once when server starts
  _ordSizeDataList.sort(function (a, b) {
    return (a.population - b.population);
  });

  // Statistics
  _data.medianCitySize = _ordSizeDataList[Math.floor(_ordSizeDataList.length / 2)].population;
  _data.quintilleOneSize = _ordSizeDataList[Math.floor(_ordSizeDataList.length / 5)].population;
  _data.quintilleTwoSize = _ordSizeDataList[Math.floor(2 * _ordSizeDataList.length / 5)].population;
  _data.quintilleThreeSize = _ordSizeDataList[Math.floor(3 * _ordSizeDataList.length / 5)].population;
  _data.quintilleFourSize = _ordSizeDataList[Math.floor(4 * _ordSizeDataList.length / 5)].population;
  _data.averageCitySize = Math.floor(_data.averageCitySize);
  _data.smallestCitySize = _ordSizeDataList[0].population;
  _data.biggestCitySize = _ordSizeDataList[_ordSizeDataList.length - 1].population;

  // Return
  return _data;
}