import { stringSimilarity } from 'string-similarity-js';

import type { Location } from '@prisma/client';

type Suggestion = Omit<
  Location,
  'id' | 'createdAt' | 'updatedAt' | 'population'
>;

const getScoresBasedOnSearchTerm = (suggestions: Suggestion[], term: string) =>
  suggestions.map(({ country, lat, long, name, state }) => ({
    name: `${name}, ${state}, ${country}`,
    lat,
    long,
    score: parseFloat(stringSimilarity(term, name).toFixed(1)),
  }));

export { getScoresBasedOnSearchTerm };
