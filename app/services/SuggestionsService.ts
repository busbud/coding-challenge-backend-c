import { inject, injectable, named } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { FirestoreRepository } from '../repositories/FirestoreRepository'
import { Suggestion, SuggestionsResponse } from '../entities/SuggestionsResponseSchema'
import { City } from '../entities/City'
import distance from 'jaro-winkler'
import haversine from 'haversine-distance'
import { Cacheable } from '@type-cacheable/core'
import { RedisClientType } from 'redis'
import { RedisAdapter, useAdapter } from '@type-cacheable/redis-adapter'

@injectable()
export class SuggestionsService {
    public static readonly NAME_PRIORITY_WEIGHT = 0.7
    public static readonly DISTANCE_PRIORITY_WEIGHT = 0.3
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger,
        @inject(TYPES.IDatabaseRepository)
        @named(TYPES.FirestoreRepository)
        private readonly db: FirestoreRepository,
        @inject(TYPES.Redis)
        private readonly redisClient: RedisClientType
    ) {
        SuggestionsService.redisClientAdapter = useAdapter(redisClient as any)
    }

    static setCacheKey: any = (args: any[]) => {
        let cacheKey = args[1]
        if (args[2] && args[3]) {
            cacheKey = cacheKey + ':' + args[2] + ':' + args[3]
        }
        return cacheKey
    }
    private static redisClientAdapter: RedisAdapter

    @Cacheable({
        cacheKey: SuggestionsService.setCacheKey,
        hashKey: 'cities',
        client: SuggestionsService.redisClientAdapter,
        ttlSeconds: 86400
    })
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
            const similarityScore = distance(
                q.toLowerCase(),
                city.name
                    .normalize('NFD')
                    .replace(/[\u0300-\u036f]/g, '')
                    .toLowerCase()
            ) //levenshtein(q, city.name).similarity
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
                distanceScore: haversineDistance
            })
        })

        // if latitude and longitude are given, then normalize distance and include it in final calculation of scores
        if (longitude && latitude) {
            suggestionsWithStringSimilarityScores.forEach((suggestion) => {
                suggestion.distanceScore = suggestion.distanceScore / maxHaversineDistance
                suggestion.score =
                    suggestion.stringSimilarity * SuggestionsService.NAME_PRIORITY_WEIGHT +
                    suggestion.distanceScore * SuggestionsService.DISTANCE_PRIORITY_WEIGHT
            })
        }

        return suggestionsWithStringSimilarityScores
    }
}
