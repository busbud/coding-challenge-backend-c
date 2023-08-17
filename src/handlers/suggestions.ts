import { matchedData } from 'express-validator';

import prisma from '../db';
import {
  getScoresBasedOnSearchTerm,
  getScoresBasedOnSearchTermAndLocation,
  getRedisClientCache,
} from './lib';
import redisClient from '../modules/redis';

import type { Request, Response } from 'express';

import type { ScoredSuggestion } from './lib';

type Search = {
  latitude?: number;
  longitude?: number;
  q: string;
};

const MINIMUM_POPULATION = 5000;
const VALID_COUNTRIES = ['CA', 'US'];

const getSuggestions = async (req: Request, res: Response) => {
  const data = matchedData(req) as Search;
  const { latitude, longitude, q: searchTerm } = data;

  try {
    const cacheResults = await getRedisClientCache(
      searchTerm,
      latitude,
      longitude,
    );

    if (cacheResults) {
      const parsedResults = JSON.parse(cacheResults);

      if (parsedResults.length === 0)
        return res.status(404).json({ suggestions: parsedResults });

      return res.status(200).json({ suggestions: parsedResults });
    }

    const suggestions = await prisma.location.findMany({
      select: {
        country: true,
        latitude: true,
        longitude: true,
        name: true,
        state: true,
      },
      where: {
        OR: [
          { ascii: { contains: searchTerm } },
          { name: { contains: searchTerm } },
        ],
        country: { in: VALID_COUNTRIES },
        population: { gt: MINIMUM_POPULATION },
      },
    });

    if (suggestions.length === 0) return res.status(404).json({ suggestions });

    let scoredSuggestions: ScoredSuggestion[];

    if (latitude && longitude) {
      scoredSuggestions = getScoresBasedOnSearchTermAndLocation(
        latitude,
        longitude,
        suggestions,
        searchTerm,
      );

      await redisClient.set(
        `${searchTerm},${latitude},${longitude}`,
        JSON.stringify(scoredSuggestions),
      );
    } else {
      scoredSuggestions = getScoresBasedOnSearchTerm(suggestions, searchTerm);

      await redisClient.set(searchTerm, JSON.stringify(scoredSuggestions));
    }

    return res.status(200).json({ suggestions: scoredSuggestions });
  } catch (error) {
    return res.status(500).json({ message: 'An unexpected error occurred.' });
  }
};

export { getSuggestions };
export type { Search };
