import { matchedData } from 'express-validator';

import prisma from '../db';

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
  const { q } = data;

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

  const filteredSuggestions = suggestions.map((suggestion) => ({
    name: `${suggestion.name}, ${suggestion.state}, ${suggestion.country}`,
    latitude: suggestion.lat,
    longitude: suggestion.long,
  }));

  res.status(200).json({ filteredSuggestions });
};

export { getSuggestions };
export type { Search };
