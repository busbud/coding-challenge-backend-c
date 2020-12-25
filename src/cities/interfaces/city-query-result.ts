import { City } from './city';

export interface CityQueryResult extends City {
  searchScore: number;
}
