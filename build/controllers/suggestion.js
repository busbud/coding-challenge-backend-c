"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getSuggestions = void 0;
const cityScoreCalc_1 = require("../lib/cityScoreCalc");
const redisClient_1 = require("../services/redisClient");
const tsvConverter_1 = require("../lib/tsvConverter");
const FIPSCodesCanada_1 = require("../data/FIPSCodesCanada");
//a function to get suggestions from the cache or calculate them if they are not in the cache yet 
// and then return them to the client
// @param req - the request object
// @param res - the response object
// @returns - the response object
// @throws - an error if the redis client is not connected
// @throws - an error if the cache key is not defined
const getSuggestions = (req, res) => __awaiter(void 0, void 0, void 0, function* () {
    var _a, _b, _c;
    console.log('getSuggestions called');
    try {
        // Load cities from TSV file
        const cities = (0, tsvConverter_1.tsvToJsonArray)('./data/cities_canada-usa.tsv');
        // Get query parameters
        const q = (_a = req.query) === null || _a === void 0 ? void 0 : _a.q;
        const qLatitude = (_b = req.query) === null || _b === void 0 ? void 0 : _b.latitude;
        const qLongitude = (_c = req.query) === null || _c === void 0 ? void 0 : _c.longitude;
        // Create a cache key based on the query parameters
        const cacheKey = `${q}-${qLatitude}-${qLongitude}`;
        //Connect to redis
        yield redisClient_1.redisClient.connect();
        // Check if the cache has the data
        const cachedResult = yield redisClient_1.redisClient.get(cacheKey);
        if (cachedResult) {
            const statusCode = JSON.parse(cachedResult).suggestions.length > 0 ? 200 : 404;
            return res.status(statusCode).json({ suggestions: JSON.parse(cachedResult) });
        }
        // If not, calculate the suggestions
        const suggestions = [];
        // Loop through the cities to find matches for the given search term
        const filteredCities = cities.filter(city => {
            const city_population = city.population;
            const cities = city.country === 'US' || city.country === 'CA' && city.population !== undefined && parseInt(city_population.replace(/,/g, '')) >= 5000;
            return cities;
        });
        for (const city of filteredCities) {
            // Get city name, state and country
            const cityName = city.name;
            const country = city.country === 'CA' ? 'Canada' : 'USA';
            const state = city.admin1;
            const state_provinceName = (0, FIPSCodesCanada_1.checkForFIPSCodes)(state, country);
            // Calculate score based on search term and optionally caller's location
            const score = (0, cityScoreCalc_1.calculateScore)(q, qLatitude, qLongitude, cityName, city);
            // Create suggestion object
            const suggestion = {
                name: `${cityName}, ${state_provinceName}, ${country}`,
                latitude: city.lat,
                longitude: city.long,
                score,
            };
            // Add suggestion to array
            suggestions.push(suggestion);
        }
        // Sort suggestions by descending score suggestions with score above 0
        const recommendedSuggestions = suggestions.filter(suggestion => suggestion.score > 0).sort((a, b) => b.score - a.score).slice(0, 10);
        // Cache the results
        yield redisClient_1.redisClient.setEx(cacheKey, 3600, JSON.stringify({ suggestions: recommendedSuggestions }));
        // Return the results
        const statusCode = recommendedSuggestions.length > 0 ? 200 : 404;
        return res.status(statusCode).json({ suggestions: recommendedSuggestions });
    }
    catch (error) {
        console.log(`Error occurred: ${error}`);
    }
    finally {
        // Disconnect from redis
        yield redisClient_1.redisClient.disconnect();
    }
});
exports.getSuggestions = getSuggestions;
//# sourceMappingURL=suggestion.js.map