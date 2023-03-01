export interface SuggestionsResponse {
    suggestions: []
}

export interface Suggestion {
    name: string
    latitude: string
    longitude: string
    score: number
}
