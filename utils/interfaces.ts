export interface City{
    population?: any;
    name?: string;
    ascii?: string;
    lat?: string;
    long?: string;
    country?: string;

}

export interface Suggestion {
    name: string;
    latitude: string;
    longitude: string;
    score: number;
}
