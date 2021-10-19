import { City } from "models/entities/City";

export interface ISuggestion {
    name: string
    latitude: string
    longitude: string
    wordScore: number
    distance: number
    realName: string
    score: number

    FromEntityCity?: (city: City, latitude: string, longitude: string, cityQuery: string) => ISuggestion
    ReHidrateFromCache?: (latitude: string, longitude: string) => void
    FromCacheData?: (cacheObject: any) => ISuggestion
}