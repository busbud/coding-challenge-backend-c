import { Request, Response } from 'express';
import { GetSuggestionParams } from '../interfaces/interfaces';
import { CitiesSuggestionService } from '../services/suggestion_service';

/** Returns router handler for  GET suggestions
 * @param {CitiesSuggestionService} suggestionService - suggestion service to be used by handler
 * */
export function setupSuggestionRoute(
  suggestionService: CitiesSuggestionService,
): (req: Request, res: Response) => void {
  return (req: Request, res: Response) => {
    const q = req.query.q as string;
    const latitude = req.query.latitude as string;
    const longitude = req.query.longitude as string;
    const params: GetSuggestionParams = {
      q,
      longitude,
      latitude,
    };
    const suggestions = suggestionService.get(params);
    res.status(200);
    suggestions.length ? res.json({ suggestions: suggestions }) : res.json({ suggestions: [] });
  };
}
