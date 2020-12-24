import { City } from './city';

export interface CityQueryResult extends City {
  score: number;
}
