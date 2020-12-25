import { Location } from 'src/location';

export interface City {
  id: string;
  name: string;
  alt_name: string;
  normalized_name: string;
  location: Location;
  population: number;
  country: string;
  state: string;
}
