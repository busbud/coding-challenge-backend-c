export interface SuggestionsResponse {
    suggestions: Suggestion[]
}

export interface Suggestion {
    name: string
    latitude: number
    longitude: number
    score: number
    stringSimilarity: number
    distance: number
}
