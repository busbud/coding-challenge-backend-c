const { normalize } = require('./textNormalization');

const buildSuggestions = function (cities, searchInput) {
    const { searchText, latitude, longitude } = searchInput;

    return cities
        .map((city) => ({
            name: [city.name, city.cc2, city.country].join(', '),
            latitude: city.lat,
            longitude: city.long,
            score: determineScore(city, searchText),
        }))
        .filter((suggestion) => suggestion.score > 0);
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
