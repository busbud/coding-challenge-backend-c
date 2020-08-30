import { Response, Request } from 'express';
import CitySearchEngine from '../services/search/CitySearchEngine';
import { RequestSearchParam, SuggestionResult } from '../types/City';

export default class SuggestionsController {

    handler(req: Request, res: Response) {
        const params = req.query as RequestSearchParam;
        if (!params?.q?.trim()) {
            res.status(404).send({
                suggestions: []
            });
            return;
        }
        if (this.isValidCoordinate(params?.latitude)) {
            res.status(400).send('Latitude must be a number');
            return;
        }
        if (this.isValidCoordinate(params?.longitude)) {
            res.status(400).send('Longitude must be a number');
            return;
        }
        CitySearchEngine.instance.findBy(params)
            .then((cities: SuggestionResult[]) => {
                res.status(cities.length ? 200 : 404).send({
                    suggestions: cities
                });
            });
    }


    isValidCoordinate(coordinate: string) {
        return coordinate && isNaN(Number.parseFloat(coordinate));
    }

}