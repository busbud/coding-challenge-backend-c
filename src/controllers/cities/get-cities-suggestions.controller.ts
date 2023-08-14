import { Response } from 'express';
import { Location } from '../../domain/models';
import { getCitiesSuggestions } from '../../services/cities/get-cities-suggestions.service';
import { TypedRequestQuery } from '../../helpers/express';
import { InvalidParamError, MissingParamError } from '../../helpers/errors';
import {
  sendBadRequest,
  sendNotFound,
  sendOk,
  sendServerError,
} from '../../helpers/http';

type GetCitiesSuggestionsQuery = {
  q: string;
  latitude: string;
  longitude: string;
};

export async function getCitiesSuggestionsController(
  req: TypedRequestQuery<GetCitiesSuggestionsQuery>,
  res: Response
) {
  try {
    const {
      q: term,
      latitude: queryLatitude,
      longitude: queryLongitude,
    } = req.query;

    if (!term) {
      return sendBadRequest(res, new MissingParamError('query'));
    }

    let location: Location | undefined;
    if (queryLatitude || queryLongitude) {
      if (!queryLatitude || !queryLongitude) {
        return sendBadRequest(
          res,
          new Error('Latitude and longitude should be provided.')
        );
      }

      const latitude = parseFloat(queryLatitude);
      const longitude = parseFloat(queryLongitude);

      if (isNaN(latitude) || Math.abs(latitude) > 90) {
        return sendBadRequest(res, new InvalidParamError('latitude'));
      }

      if (isNaN(longitude) || Math.abs(longitude) > 180) {
        return sendBadRequest(res, new InvalidParamError('longitude'));
      }

      location = {
        latitude,
        longitude,
      };
    }

    const suggestions = await getCitiesSuggestions(req.context, term, location);

    if (!suggestions.length) {
      return sendNotFound(res, {
        suggestions: [],
      });
    }

    sendOk(res, {
      suggestions,
    });
  } catch (error) {
    let err;
    if (error instanceof Error) {
      err = error;
    }
    sendServerError(res, err);
  }
}
