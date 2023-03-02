export interface IDatabaseRepository {
    fetchCities(requestId: string, q: string, longitude?: number, latitude?: number): void
}
