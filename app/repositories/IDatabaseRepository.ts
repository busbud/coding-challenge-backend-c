export interface IDatabaseRepository {
    fetchSuggestions(requestId: string, q: string, longitude?: number, latitude?: number): void
}
