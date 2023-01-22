export type Coordinates = {
  longitude: number;
  latitude: number;
};

export interface CitiesDTO extends Coordinates {
  name: string;
  score: number;
}

export interface GetCitiesByQueryParamParam {
  cityName: string;
  longitude?: string;
  latitude?: string;
}

export interface GetSuggestionAccuracyByLatAndLongParam {
  suggestedLocationCoordinates: Coordinates;
  latitude: number;
  longitude: number;
}

export interface LocationBreakdownResponse {
  city?: string;
  state?: string;
  country?: string;
}

export interface ICitiesService {
  getCitiesByQueryParam(param: GetCitiesByQueryParamParam): Promise<CitiesDTO[]>;
}

export const ACCEPTED_COUNTRIES_ENUM: { [key: string]: string } = {
  US: "USA",
  CA: "Canada",
};
export const ACCEPTED_COUNTRIES = Object.keys(ACCEPTED_COUNTRIES_ENUM);
export const ACCEPTED_MIN_POPULATION = 5000;
