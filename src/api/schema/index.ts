export interface City {
  id: string;
  name: string;
  asciiName: string;
  latitude: number;
  longitude: number;
}

export interface CityResult extends City { score: number };

