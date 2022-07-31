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