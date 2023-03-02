import { inject, injectable, named } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { FirestoreRepository } from '../repositories/FirestoreRepository'
import { Suggestion, SuggestionsResponse } from '../entities/SuggestionsResponseSchema'
import { City } from '../entities/City'
import levenshtein from 'damerau-levenshtein'
import haversine from 'haversine-distance'

@injectable()
export class SuggestionsService {
    public static readonly NAME_PRIORITY_WEIGHT = 0.7
    public static readonly DISTANCE_PRIORITY_WEIGHT = 0.3
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger,
        @inject(TYPES.IDatabaseRepository)
        @named(TYPES.FirestoreRepository)
        private readonly db: FirestoreRepository
    ) {}

    public async fetchSuggestions(
        requestId: string,
        q: string,
        longitude?: number,
        latitude?: number
    ): Promise<SuggestionsResponse> {
        const cities = await this.db.fetchCities()

        const suggestions = this.calculateSuggestionsScores(cities, q, longitude, latitude)

        suggestions.sort((s1, s2) => s2.score - s1.score)
        return {
            suggestions
        }
    }

    private calculateSuggestionsScores(
        cities: City[],
        q: string,
        longitude?: number,
        latitude?: number
    ): Suggestion[] {
        let maxHaversineDistance = 0
        const suggestionsWithStringSimilarityScores: Suggestion[] = []
        cities.map((city) => {
            const similarityScore = levenshtein(q, city.name).similarity
            let haversineDistance = 0
            if (longitude && latitude) {
                haversineDistance = haversine(
                    { latitude, longitude },
                    { latitude: city.latitude, longitude: city.longitude }
                )
                if (maxHaversineDistance < haversineDistance) {
                    maxHaversineDistance = haversineDistance
                }
            }
            suggestionsWithStringSimilarityScores.push({
                name: city.name,
                latitude: city.latitude,
                longitude: city.longitude,
                score: similarityScore,
                stringSimilarity: similarityScore,
                distance: haversineDistance
            })
        })

        // if latitude and longitude are given, then normalize distance and include it in final calculation of scores
        if (longitude && latitude) {
            suggestionsWithStringSimilarityScores.forEach((suggestion) => {
                suggestion.distance = suggestion.distance / maxHaversineDistance
                suggestion.score =
                    suggestion.stringSimilarity * SuggestionsService.NAME_PRIORITY_WEIGHT +
                    suggestion.distance * SuggestionsService.DISTANCE_PRIORITY_WEIGHT
            })
        }

        return suggestionsWithStringSimilarityScores
    }
}
