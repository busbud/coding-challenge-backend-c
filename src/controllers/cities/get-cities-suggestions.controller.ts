import { Response } from 'express';
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
};

export async function getCitiesSuggestionsController(
  req: TypedRequestQuery<GetCitiesSuggestionsQuery>,
  res: Response
) {
  try {
    const query = req.query.q;

    if (!query) {
      sendBadRequest(res, new MissingParamError('query'));
    }

    const suggestions = await getCitiesSuggestions(req.context.prisma, query);

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
    sendServerError(res, error);
  }
}
