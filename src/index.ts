import express, { Express, Request, Response } from 'express';
import { getCitiesSuggestions } from './utils/searchEngine'
import { getCitiesSearchableObject } from './utils/dataParser'

const port = process.env.PORT;

let citiesSearchableObject: LargeCity[] = getCitiesSearchableObject();
let citiesSuggestionsCache: CitiesSuggestionsCache = {};

const app: Express = express();

app.get('/suggestions', (req: TypedRequestQuery<{ q: string, latitude: string, longitude: string }>, res: Response) => {
  const searchParams: SearchParams = { ...req.query };
  const citiesSuggestions: CitySuggestion[] = getCitiesSuggestions(searchParams, citiesSearchableObject, citiesSuggestionsCache);
  if (citiesSuggestions.length !== 0) {
    res.status(200);
    res.send(citiesSuggestions);
  } else {
    res.status(400);
    res.send([]);
  }
});

// start the express server
app.listen(port, () => {
  // tslint:disable-next-line:no-console
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});

export default app;

