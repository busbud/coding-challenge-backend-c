export interface City {
  id: string;
  name: string;
  asciiName: string;
  territory?: string;
  country?: string;
  latitude: number;
  longitude: number;
}

export interface CityResult extends Omit<City, 'id' | 'asciiName' | 'territory' | 'country'> { score: number };

