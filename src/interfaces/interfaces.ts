export interface ICityRawData {
  id: string;
  name: string;
  ascii: string;
  alt_name: string | undefined;
  latitude: string;
  longitude: string;
  feat_class: string;
  feat_code: string;
  country: string;
  cc2: string | undefined;
  admin1: string | undefined;
  admin2: string | undefined;
  admin3: string | undefined;
  admin4: string | undefined;
  population: number;
  elevation: number | undefined;
  dem: number;
  timezone: string;
  modified_at: string;
}

export interface IGetCitySuggestion {
  name: string;
  latitude: string;
  longitude: string;
  score: number;
}

export interface GetSuggestionParams {
  q: string;
  latitude?: string;
  longitude?: string;
}

export interface IApiConfig {
  countriesSupported: string[];
  populationLimit: number;
}