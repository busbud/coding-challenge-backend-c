import { Response, Request } from 'express';
import CitySearchEngine from '../services/search/CitySearchEngine';
import { RequestSearchParam, SuggestionResult } from '../types/City';
import { HTTP_STATUS_CODE } from '../constants/httpStatus';

export default class SuggestionsController {

    handler(req: Request, res: Response): Response  {
        const params = req.query as RequestSearchParam;
        const hasQuery = params?.q?.trim();

        if (!hasQuery) {
            res.status(HTTP_STATUS_CODE.NOT_FOUND).send({ suggestions: [] });
            return;
        }
        if (this.isValidCoordinate(params?.latitude)) {
            res.status(HTTP_STATUS_CODE.ERROR).send('Latitude must be a number');
            return;
        }
        if (this.isValidCoordinate(params?.longitude)) {
            res.status(HTTP_STATUS_CODE.ERROR).send('Longitude must be a number');
            return;
        }
        const cities: SuggestionResult[] = CitySearchEngine.instance.findBy(params)
        return res.status(cities.length ? HTTP_STATUS_CODE.OK : HTTP_STATUS_CODE.NOT_FOUND).send({ suggestions: cities });
    }


    isValidCoordinate(coordinate: string): boolean {
        return coordinate && isNaN(Number.parseFloat(coordinate));
    }

}