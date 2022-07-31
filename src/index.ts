import express, { Express, Response } from 'express';
import { getCitiesSuggestions } from './helpers/searchEngine.js'
import { getCitiesSearchableObject } from './helpers/dataParser.js'

const port = process.env.PORT || 2345;

let citiesSearchableObject: City[] = getCitiesSearchableObject();
let citiesSuggestionsCache: CitiesSuggestionsCache = {};

export const app: Express = express();

app.get('/suggestions', (req: TypedRequestQuery<{ q: string, latitude: string, longitude: string }>, res: Response) => {
  const searchParams: SearchParams = { name: req.query.q, latitude: req.query.latitude, longitude: req.query.longitude };
  const citiesSuggestions: CitySuggestion[] = getCitiesSuggestions(searchParams, citiesSearchableObject, citiesSuggestionsCache);
  if (citiesSuggestions.length !== 0) {
    res.status(200);
    res.send(citiesSuggestions);
  } else {
    res.status(404);
    res.send([]);
  }
});

app.listen(port, () => {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});