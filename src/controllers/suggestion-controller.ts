import { Request, Response } from 'express';
import { RedisClient } from '../clients/redis-client';
import { SuggestionService } from '../services/suggestion-service';
import { buildSuggestionCacheKey } from '../middlewares/suggestion-middleware';

export class SuggestionController {
  constructor(private suggestionService: SuggestionService, private redisClient: RedisClient) {}

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

    await this.redisClient.setKey(
      buildSuggestionCacheKey(term as string, location?.lat, location?.long),
      suggestions,
    );

    res.json({ suggestions });
  };
}
