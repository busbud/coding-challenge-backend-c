module.exports = Object.freeze({
	// For name scoring operation; should add to 1
	nameCompletionScoreWeight: 0.1,
	numberOfMatchesScoreWeight: 0.9,

	// For geo + name scoring operation; should add to 1
	findNearStartsWithNameWeight: 0.1,
	findNearStartsWithGeoWeight: 0.9,
	geoRadiusBoundaries: [0, 10, 22, 46, 100, 215, 464, 1000, 2154, 4642, Infinity]
});