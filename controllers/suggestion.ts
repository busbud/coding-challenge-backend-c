import { Request, Response } from 'express';
import { Suggestion } from '../utils/interfaces';
import { calculateScore } from '../lib/cityScoreCalc';
import { redisClient } from '../services/redisClient';
import { tsvToJsonArray } from '../lib/tsvConverter';
import { checkForFIPSCodes } from '../data/FIPSCodesCanada';


//a function to get suggestions from the cache or calculate them if they are not in the cache yet 
// and then return them to the client
// @param req - the request object
// @param res - the response object
// @returns - the response object
// @throws - an error if the redis client is not connected
// @throws - an error if the cache key is not defined



export const getSuggestions = async (req: Request, res: Response) => {

    console.log('getSuggestions called')
    try {
        // Load cities from TSV file
        const cities = tsvToJsonArray('./data/cities_canada-usa.tsv');

        // Get query parameters
        const q = req.query?.q as string;
        const qLatitude = req.query?.latitude as string;
        const qLongitude = req.query?.longitude as string;

        // Create a cache key based on the query parameters
        const cacheKey = `${q}-${qLatitude}-${qLongitude}`;

        //Connect to redis
        await redisClient.connect();

        // Check if the cache has the data
        const cachedResult = await redisClient.get(cacheKey);
        if (cachedResult) {
            const statusCode = JSON.parse(cachedResult).suggestions.length > 0 ? 200 : 404;
            return res.status(statusCode).json(JSON.parse(cachedResult));
        }

        // If not, calculate the suggestions
        const suggestions: Suggestion[] = [];

        // Loop through the cities to find matches for the given search term
        const filteredCities = cities.filter(city => {
            const city_population = city.population as string;
            const cities = city.country === 'US' || city.country === 'CA' && city.population !== undefined && parseInt(city_population.replace(/,/g, '')) >= 5000;
            return cities;
        })
        for (const city of filteredCities) {
            // Get city name, state and country
            const cityName = city.name as string;
            const country = city.country === 'CA' ? 'Canada' : 'USA';
            const state = city.admin1 as string;
            const state_provinceName = checkForFIPSCodes(state, country);

            // Calculate score based on search term and optionally caller's location
            const score = calculateScore(q, qLatitude, qLongitude, cityName, city);

            // Create suggestion object
            const suggestion: Suggestion = {
                name: `${cityName}, ${state_provinceName}, ${country}`,
                latitude: city.lat as string,
                longitude: city.long as string,
                score,
            };

            // Add suggestion to array
            suggestions.push(suggestion);
        }

        // Sort suggestions by descending score suggestions with score above 0
        const recommendedSuggestions = suggestions.filter(suggestion => suggestion.score > 0).sort((a, b) => b.score - a.score).slice(0, 10);

        // Cache the results
        await redisClient.setEx(cacheKey, 3600, JSON.stringify({ suggestions: recommendedSuggestions }));

        // Return the results
        const statusCode = recommendedSuggestions.length > 0 ? 200 : 404;
        return res.status(statusCode).json({ suggestions: recommendedSuggestions } );
    } catch (error) {
        console.log(`Error occurred: ${error}`);
    } finally {
        // Disconnect from redis
        await redisClient.disconnect();
    }
};
