import express, { Request, Response } from 'express';
const router = express.Router();

import redis from 'redis';
import { promisify } from 'util';



import { tsvToJsonArray } from '../lib/tsvConverter';
import { City, Suggestion } from '../utils/interfaces';
import { calculateScore } from '../lib/cityScoreCalc';


// Load cities from TSV file
const cities = tsvToJsonArray('./data/cities_canada-usa.tsv');



router.get("/", (req, res) => {res.send(cities);});



router.get('/suggestions', (req: Request, res: Response) => {

    const q = req.query?.q as string;
    const qLatitude = req.query?.latitude as string;
    const qLongitude = req.query?.longitude as string;




    // Check if the search term is in the cache
 
    

    const suggestions: Suggestion[] = [];

    console.log("Cites length: " + cities.length)

    // Loop through the cities to find matches for the given search term
    for (let i = 0; i < cities.length; i++) {
        const city = cities[i];
        const city_population = city.population as string;

        if (city_population !== undefined) {
            const population = parseInt(city_population.replace(/,/g, ''));

            // Filter cities with a population above 5000 people in the USA and Canada
            if (population >= 5000 && (city.country === 'US' || city.country === 'CA')) {
                const cityName = city.name as string;
                const state = city.admin1;
                const country = city.country;

                // Calculate score based on search term and optionally caller's location
                const score = calculateScore(q, qLatitude, qLongitude, cityName, city);


                // Create suggestion object
                const suggestion: Suggestion = {
                    name: `${cityName}, ${state}, ${country}`,
                    latitude: city.lat as string,
                    longitude: city.long as string,
                    score,
                };


                suggestions.push(suggestion);
            }
        }

        
    }

    // Sort suggestions by descending score suggestions with score above 0
    const recommendedSuggestions = suggestions.filter(suggestion => suggestion.score > 0).sort((a, b) => b.score - a.score).slice(0, 10);
    res.json({ suggestions: recommendedSuggestions });
});
















module.exports = router;