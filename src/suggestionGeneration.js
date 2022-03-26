const { normalize } = require('./textNormalization');

module.exports.buildSuggestions = function (cities, searchInput) {
    return cities
        .filter((city) => city.population > 5000)
        .map((city) => ({
            name: [city.name, city.country].join(', '),
            latitude: city.lat,
            longitude: city.long,
            score: determineScore(city, searchInput),
        }))
        .filter((suggestion) => suggestion.score > 0)
        .sort((left, right) => right.score - left.score);
};

function determineScore(city, searchInput) {
    const { searchText, latitude, longitude } = searchInput;

    const weightedScores = [
        0.8 * determineTextScore(city, searchText),
        0.2 * determineGeographicalScore(city, latitude, longitude),
    ];

    return weightedScores.reduce((sum, value) => sum + value, 0);
}

function determineTextScore(city, searchText) {
    const normalizedName = normalize(city.name);
    searchText = normalize(searchText);

    const nameMatches = normalizedName.search(searchText) >= 0;

    return nameMatches ? searchText.length / normalizedName.length : 0;
}

// This is obviously a terrible way of calculating geographical distance and a terrible algorithm
// But it will have to do because I'm out of time
function determineGeographicalScore(city, latitude, longitude) {
    if (!latitude || !longitude) {
        return 0;
    }

    const geographicalDistance = Math.sqrt(
        Math.pow(latitude - city.latitude, 2) +
            Math.pow(longitude - city.longitude, 2)
    );

    const maxDistance = 150; //arbitrary number

    //Some kind of exponential algorithm adding a lot more weight to close matches than farther matches would be interesting here
    return (1 - Math.min(geographicalDistance, maxDistance)) / maxDistance;
}
