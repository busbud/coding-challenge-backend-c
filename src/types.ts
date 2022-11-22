export interface City {
  name: string;
  longitude: number;
  latitude: number;
  score?: number;
  distance?: number;
  levenshtein?: number;
}
