var constants = require('./constants');

module.exports.geoScore = function(bins, targetScoreKey) {
    var binScoreStep = 1 / (constants.geoRadiusBoundaries.length + 1),
    	binScoreStepValue = 0,
    	innerBinScoreStep = 0,
    	innerBinScoreValue = 0,
    	bin = null,
    	suggestion = null;
    for(var i = 0, len=bins.length; i<len; ++i) {
        bin = bins[i];
        binScoreStepValue = 1 -
        	(constants.geoRadiusBoundaries.indexOf(bin._id) -
        	 constants.geoRadiusBoundaries.indexOf(bins[0]._id))*binScoreStep;
        innerBinScoreStep = binScoreStep/(bin.count + 1);
        innerBinScoreValue = bin.count > 1 ? innerBinScoreStep : 0;
        for(var j = 0, _len=bin.suggestions.length; j<_len; ++j) {
            suggestion = bin.suggestions[j];
            suggestion[targetScoreKey] = Math.round((binScoreStepValue - innerBinScoreValue)*100)/100;
            innerBinScoreValue = innerBinScoreValue + innerBinScoreStep;
        }
    }
    return bins;
}

module.exports.nameScore = function(suggestions, nameCompletionScoreKey, targetScoreKey) {
    // Case of absolute confidence
    if (suggestions.length === 1) {
        suggestions[0][targetScoreKey] = 1;
        suggestions[0][nameCompletionScoreKey] = undefined;
        return suggestions;
    }
	// compute the factor inversely proportional to the number of matches
	var numberOfMatchesConfidenceFactor = (1/suggestions.length) *
		constants.numberOfMatchesScoreWeight;

	for(var i = 0, len=suggestions.length; i<len; ++i) {
		// compute the city's name factor based on the remaining unmatched portion
		var nameCompletionConfidenceFactor = (suggestions[i][nameCompletionScoreKey]) *
		constants.nameCompletionScoreWeight;

		// sum both factors
        suggestions[i][targetScoreKey] = Math.round((numberOfMatchesConfidenceFactor +
        	nameCompletionConfidenceFactor) * 100) / 100;

        suggestions[i][nameCompletionScoreKey] = undefined;
    }
    return suggestions;
}