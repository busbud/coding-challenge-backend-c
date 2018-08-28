const geolib = require('geolib');
const StringManipulationService = require('../utils/string-manipulation.service');
const config = require('../config/config');

const utilityService = new StringManipulationService();

class SuggestionEngineService {
    constructor(databaseService) {
        this.cities = databaseService.cities;
        this.suggestedCityMap = databaseService.suggestedCityMap;
    }

    findCities(cityName, latitude, longitude) {
        cityName = utilityService.removeDashAndWhiteSpace(cityName);
        if (!this.suggestedCityMap[cityName.toUpperCase()]) {
            return [];
        }
        const suggestions = this.suggestedCityMap[cityName.toUpperCase()].map(city => {
            const currentCity = this.cities[city.id];
            return {
                name: `${currentCity.name}, ${currentCity.state}, ${currentCity.country}`,
                latitude: currentCity.latitude,
                longitude: currentCity.longitude,
                score: this.getScore(currentCity, cityName, latitude, longitude, city.nameScoreLength)
            };
        });

        const sortedByScore = suggestions.sort((a, b) => b.score - a.score).slice(0, config.suggestionEngine.maxSuggestions);

        return sortedByScore;
    }

    /**
     * Get score for particular city suggestion
     *
     * @param {*} currentCity current city obj
     * @param {*} nameSearchKey request name search key
     * @param {*} latitudeSearchKey request latitude search key
     * @param {*} longitudeSearchKey request longitude search key
     * @param {*} indexKeyLength the length of the original index key that was used when indexing
     * @returns score - number between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
     * @memberof SuggestionEngineService
     */
    getScore(currentCity, nameSearchKey, latitudeSearchKey, longitudeSearchKey, indexKeyLength) {

        const nameScore = nameSearchKey.length / indexKeyLength;

        if (typeof latitudeSearchKey === "undefined" || typeof longitudeSearchKey === "undefined")
            return nameScore;
        var distance = geolib.getDistance({
            latitude: latitudeSearchKey,
            longitude: longitudeSearchKey
        }, {
            latitude: currentCity.latitude,
            longitude: currentCity.longitude
        });

        // ignore distance if bigger than max radius
        if (distance > config.suggestionEngine.maxDistanceRadiusInMeter)
            return nameScore;

        const distanceScore = distance === 0 ? 1 : 1 / distance;
        const score = (nameScore === 1 && distanceScore === 1) ? 1 : (distanceScore * nameScore);
        return score;
    }
}


module.exports = SuggestionEngineService;