import { Request, Response, NextFunction } from 'express';
import { RedisClient } from '../clients/redis-client';
import { GetSuggestionResult } from '../services/suggestion-service';

export function buildSuggestionCacheKey(term: string, lat: number = 0, long: number = 0) {
  const key = `${term.replace(/\s/g, '-')}:${lat}:${long}`;
  return key;
}

export class SuggestionMiddleware {
  constructor(private redisClient: RedisClient) {}

  checkSuggestionCache = async (req: Request, res: Response, next: NextFunction) => {
    const key = buildSuggestionCacheKey(req.query.q as string, Number(req.query.lat || 0), Number(req.query.long || 0));

    console.log('Looking for key:', key);
    const suggestions = await this.redisClient.getKey<GetSuggestionResult[]>(key);

    if (suggestions) {
      console.log('Found suggestions in cache:', JSON.stringify(suggestions));
      return res.json({ suggestions });
    }

    console.log('No suggestions found in cache');
    return next();
  };
}
