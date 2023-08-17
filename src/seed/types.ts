import type { Location } from '@prisma/client';

type Attributes =
  | 'admin1'
  | 'ascii'
  | 'country'
  | 'id'
  | 'lat'
  | 'long'
  | 'name'
  | 'population';

type CSVLocation = Omit<Location, 'createdAt' | 'updatedAt'>;

export type { Attributes, CSVLocation };
