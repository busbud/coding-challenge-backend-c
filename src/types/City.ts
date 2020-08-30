import { FuzzyVector } from './Fuzzy';

export interface City {
    name: string;
    latitude: number;
    longitude: number;
    country: string;
    province: string;
    population: number;
    nGram?: FuzzyVector[];
    magnitude?: number;
}


export interface SuggestionResult {
    name: string;
    latitude: number;
    longitude: number;
    score: number;
}

export type RequestSearchParam = {
    q: string;
    latitude?: string;
    longitude?: string;
}

export interface CityParam {
    name: string;
    latitude: number;
    longitude: number;
}