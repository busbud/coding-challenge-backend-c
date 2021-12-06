import { Request, Response } from 'express';
import { SuggestionService } from '../services/suggestion-service';

export class SuggestionController {
  constructor(private suggestionService: SuggestionService) {}

  getSuggestions = async (req: Request, res: Response): Promise<void> => {
    let location;

    if (req.query.lat && req.query.long) {
      location = {
        lat: Number(req.query.lat),
        long: Number(req.query.long),
      };
    }

    const { q: term } = req.query;
    const suggestions = await this.suggestionService.getSuggestions(term as string, location);

    res.json({ suggestions });
  };
}
