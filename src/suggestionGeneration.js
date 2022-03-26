const { normalize } = require('./textNormalization');

const buildSuggestions = function (cities, searchInput) {
    const { searchText, latitude, longitude } = searchInput;

    return cities
        .filter((city) => city.population > 5000)
        .map((city) => ({
            name: [city.name, city.country].join(', '),
            latitude: city.lat,
            longitude: city.long,
            score: determineScore(city, searchText),
        }))
        .filter((suggestion) => suggestion.score > 0)
        .sort((left, right) => right.score - left.score);
};

const determineScore = function (city, searchText) {
    const normalizedName = normalize(city.name);
    searchText = normalize(searchText);

    const nameMatches = normalizedName.search(searchText) >= 0;

    return nameMatches ? searchText.length / normalizedName.length : 0;
};

module.exports = {
    buildSuggestions,
    determineScore,
};
