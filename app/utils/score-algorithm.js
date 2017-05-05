var constants = require('./constants');
var geoScoreBinStep = constants.geoScoreMax/constants.geoRadiusBoundaries.length;

module.exports.geoScore = function(bins) {
    var geoScoreStep = 0;
    var partialBinRange = 0;
    var currBinRange = 0;
    var bin = null;
    var city = null;
    for(var i = 0, len=bins.length; i<len; ++i) {
        bin = bins[i];
        geoScoreStep = constants.geoScoreMax - constants.geoRadiusBoundaries.indexOf(bin._id)*geoScoreBinStep;
        partialBinRange = geoScoreBinStep/(bin.count + 1);
        currBinRange = bin.count > 1 ? partialBinRange : 0;
        for(var j = 0, _len=bin.cities.length; j<_len; ++j) {
            city = bin.cities[j];
            city.geoScore = geoScoreStep - currBinRange;
            currBinRange =  currBinRange + partialBinRange;
        }
    }
    return bins;
}