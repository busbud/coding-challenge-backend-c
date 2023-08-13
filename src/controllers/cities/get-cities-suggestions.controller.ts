import { Response } from 'express';
import { Location } from '../../domain/models';
import { getCitiesSuggestions } from '../../services/cities/get-cities-suggestions.service';
import { TypedRequestQuery } from '../../helpers/express';
import { MissingParamError } from '../../helpers/errors';
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
    const { q: query, latitude, longitude } = req.query;

    if (!query) {
      sendBadRequest(res, new MissingParamError('query'));
    }

    let location: Location | undefined;
    if (latitude && longitude) {
      location = {
        latitude: parseFloat(latitude),
        longitude: parseFloat(longitude),
      };
    }

    const suggestions = await getCitiesSuggestions(
      req.context,
      query,
      location
    );

    if (!suggestions.length) {
      sendNotFound(res, {
        suggestions: [],
      });

      return;
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
