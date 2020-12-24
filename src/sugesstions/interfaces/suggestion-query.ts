import { Location } from 'src/location';

export interface SuggestionQuery {
  query: string;
  location?: Location;
}
