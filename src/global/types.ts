interface CanadianStates {
    [key: string]: string
}
interface TypedRequestQuery<T> extends Express.Request {
    query: T
}
interface SearchParams {
    q: string,
    latitude: string,
    longitude: string
}
interface LargeCity {
    name: string,
    country: string,
    state: string,
    latitude: string,
    longitude: string
}
interface CitiesSuggestionsCache {
    [query: string]: CitySuggestion[]
}
interface CitySuggestion {
    name: string,
    lattitude: string,
    longitude: string,
    score: Number
}