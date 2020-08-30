export interface FuzzyVector {
    nGram: string;
    count: number;
}

export interface SearchCityFuzzy {
    searchGram: Map<string, number>,
    magnitude: number,
    latitude?: string,
    longitude?: string
}