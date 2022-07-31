interface CanadianStates {
    [key: string]: string
}
interface City {
    name: string,
    country: string,
    state: string,
    latitude: string,
    longitude: string
}
interface CitiesDistance {
    [key: number]: number
}
interface CitySuggestion {
    name: string,
    latitude: string,
    longitude: string,
    score: number
}
interface CitiesSuggestionsCache {
    [query: string]: CitySuggestion[]
}
interface SearchParams {
    name: string,
    latitude: string,
    longitude: string
}
interface TypedRequestQuery<T> extends Express.Request {
    query: T
}