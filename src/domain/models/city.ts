import { City } from '@prisma/client';

type CitySuggestion = {
  name: string;
  score: number;
} & Pick<City, 'latitude' | 'longitude'>;

export { City, CitySuggestion };
