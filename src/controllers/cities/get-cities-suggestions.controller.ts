import { Request, Response } from 'express';
import { getCitiesSuggestions } from '../../services/cities/get-cities-suggestions.service';

export async function getCitiesSuggestionsController(
  req: Request,
  res: Response
) {
  const suggestions = await getCitiesSuggestions(req.context.prisma);

  if (!suggestions.length) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    res.end(
      JSON.stringify({
        suggestions: [],
      })
    );

    return;
  }
}
