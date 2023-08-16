import { matchedData } from 'express-validator';

import prisma from '../db';
import { getScoresBasedOnSearchTerm } from './lib';

import type { Request, Response } from 'express';

type Search = {
  latitude?: number;
  longitude?: number;
  q: string;
};

const MINIMUM_POPULATION = 5000;
const VALID_COUNTRIES = ['CA', 'US'];

const getSuggestions = async (req: Request, res: Response) => {
  const data = matchedData(req) as Search;
  const { latitude, longitude, q } = data;

  try {
    const suggestions = await prisma.location.findMany({
      select: {
        country: true,
        lat: true,
        long: true,
        name: true,
        state: true,
      },
      where: {
        name: { contains: q },
        country: { in: VALID_COUNTRIES },
        population: { gt: MINIMUM_POPULATION },
      },
    });

    if (latitude && longitude) {
      console.log('to implement');
    } else {
      res
        .status(200)
        .json({ suggestions: getScoresBasedOnSearchTerm(suggestions, q) });
    }
  } catch (error) {
    // TODO: Implement custom error handling
    res.status(500).json({ message: 'Something went wrong...' });
  }
};

export { getSuggestions };
export type { Search };
