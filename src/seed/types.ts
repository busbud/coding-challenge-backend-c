import type { Location } from '@prisma/client';

type Attributes =
  | 'id'
  | 'name'
  | 'lat'
  | 'long'
  | 'country'
  | 'admin1'
  | 'population';

type CSVLocation = Omit<Location, 'createdAt' | 'updatedAt'>;

export type { Attributes, CSVLocation };
