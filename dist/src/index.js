import express from 'express';
import { getCitiesSuggestions } from './helpers/searchEngine.js';
import { getCitiesSearchableObject } from './helpers/dataParser.js';
const port = process.env.PORT || 2345;
let citiesSearchableObject = getCitiesSearchableObject();
let citiesSuggestionsCache = {};
const app = express();
app.get('/suggestions', (req, res) => {
    const searchParams = { name: req.query.q, latitude: req.query.latitude, longitude: req.query.longitude };
    const citiesSuggestions = getCitiesSuggestions(searchParams, citiesSearchableObject, citiesSuggestionsCache);
    if (citiesSuggestions.length !== 0) {
        res.status(200);
        res.send(citiesSuggestions);
    }
    else {
        res.status(404);
        res.send([]);
    }
});
app.listen(port, () => {
    console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});
export default app;
