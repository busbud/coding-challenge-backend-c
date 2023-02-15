import express, { Request, Response } from 'express';
const router = express.Router();


import { tsvToJsonArray } from '../lib/tsvConverter';
import { Suggestion } from '../utils/interfaces';
import { calculateScore } from '../lib/cityScoreCalc';


// Load cities from TSV file
const cities = tsvToJsonArray('./data/cities_canada-usa.tsv');



router.get("/", (req, res) => {res.send(cities);});



router.get('/suggestions', (req: Request, res: Response) => {

    const q = req.query?.q as string;
    const qLatitude = req.query?.latitude as string;
    const qLongitude = req.query?.longitude as string;


    const recommendedSuggestions = cities
        .filter(city => {
            const city_population = city.population as string;
            const population = parseInt(city_population?.replace(/,/g, ''));
            return population >= 5000 && (city.country === 'US' || city.country === 'CA');
        })
        .map(city => {
            const cityName = city.name as string;
            const state = city.admin1;
            const country = city.country;
            const score = calculateScore(q, qLatitude, qLongitude, cityName, city);

            return {
                name: `${cityName}, ${state}, ${country}`,
                latitude: city.lat as string,
                longitude: city.long as string,
                score,
            };
        })
        .filter(suggestion => suggestion.score > 0)
        .sort((a, b) => b.score - a.score)
        .slice(0, 10);

    res.json({ suggestions: recommendedSuggestions });

});
















module.exports = router;