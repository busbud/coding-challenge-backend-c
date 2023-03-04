/* eslint sonarjs/cognitive-complexity: 0 */
import { expect } from 'chai'
import { createSandbox, SinonStubbedInstance } from 'sinon'
import { FirestoreRepository } from '../../../app/repositories/FirestoreRepository'
import { SuggestionsService } from '../../../app/services/SuggestionsService'
import { kernel } from '../../../app/Kernel'
import { Logger } from 'pino'
import { mockCities } from '../../fixtures/FirestoreFetchCitiesReturns'
import { RedisClientType } from 'redis'
import { City } from '../../../app/entities/City'
import { Suggestion } from '../../../app/entities/SuggestionsResponseSchema'

const sandbox = createSandbox()
let dbRepository: SinonStubbedInstance<FirestoreRepository>
let suggestionsService: SuggestionsService
let logger: SinonStubbedInstance<Logger>
let redisClient: SinonStubbedInstance<RedisClientType>
const requestId = 'fake-request-id'

describe('SuggestionsService', () => {
    beforeEach(() => {
        sandbox.restore()
        kernel.unbindAll()

        dbRepository = sandbox.createStubInstance(FirestoreRepository)

        suggestionsService = new SuggestionsService(logger, dbRepository, redisClient)
    })
    describe('fetchSuggestions', () => {
        describe('when only q param is passed', () => {
            it('should call Db repository and return sorted same number of suggestions as the cities returned by repository with string similarity scores same as suggestions score', async () => {
                const q = 'lk'
                dbRepository.fetchCities.resolves(mockCities)
                const response = await suggestionsService.fetchSuggestions(requestId, q)

                expect(dbRepository.fetchCities.callCount).to.equal(1)
                expect(response.suggestions.length).to.equal(mockCities.length)
                for (const suggestion of response.suggestions) {
                    const found = isSuggesionCityInArrayOfCities(suggestion, mockCities)
                    // if latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                    const isScoreBasedOnStringSimilarityCompletely =
                        suggestion.score === suggestion.stringSimilarity

                    expect(found && isScoreBasedOnStringSimilarityCompletely).to.equal(true)
                }
            })
            it('should call Db repository and return sorted zero suggestions if 0 cities are returned by repository', async () => {
                const q = 'lk'
                dbRepository.fetchCities.resolves([])
                const response = await suggestionsService.fetchSuggestions(requestId, q)

                expect(dbRepository.fetchCities.callCount).to.equal(1)
                expect(response.suggestions.length).to.equal(0)
            })
        })
        describe('when only q param and only longitude are passed', () => {
            it('should call Db repository and return sorted same number of suggestions as the cities returned by firestore repo with string similarity scores same as suggestions score', async () => {
                const q = 'mo'
                dbRepository.fetchCities.resolves(mockCities)

                // test with q and longitude
                const longitude = -11.89
                const response = await suggestionsService.fetchSuggestions(requestId, q, longitude)

                expect(response.suggestions.length).to.equal(mockCities.length)
                expect(dbRepository.fetchCities.callCount).to.equal(1)
                for (const suggestion of response.suggestions) {
                    const found = isSuggesionCityInArrayOfCities(suggestion, mockCities)
                    // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                    const isScoreBasedOnStringSimilarityCompletely =
                        suggestion.score === suggestion.stringSimilarity

                    expect(found && isScoreBasedOnStringSimilarityCompletely).to.equal(true)
                }
            })
        })
        describe('when only q param and only latitude are passed', () => {
            it('should call Db repository and return sorted same number of suggestions as the cities returned by repository with string similarity scores same as suggestions score', async () => {
                const q = 'mo'
                dbRepository.fetchCities.resolves(mockCities)

                // test with q and latitude
                const latitude = -33.89
                const response = await suggestionsService.fetchSuggestions(
                    requestId,
                    q,
                    null,
                    latitude
                )
                for (const suggestion of response.suggestions) {
                    const found = isSuggesionCityInArrayOfCities(suggestion, mockCities)
                    // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                    const isScoreBasedOnStringSimilarityCompletely =
                        suggestion.score === suggestion.stringSimilarity

                    expect(found && isScoreBasedOnStringSimilarityCompletely).to.equal(true)
                }
                expect(dbRepository.fetchCities.callCount).to.equal(1)
                expect(response.suggestions.length).to.equal(mockCities.length)
            })
        })
        describe('when q param and both longitude and latitude are passed', () => {
            it('should call Db repository and return sorted same number of suggestions as the cities returned by repository with suggestions score weighted combination of string similarity and ditance scores', async () => {
                const q = 'mo'
                const latitude = -33.89
                const longitude = 11.76
                dbRepository.fetchCities.resolves(mockCities)
                const response = await suggestionsService.fetchSuggestions(
                    requestId,
                    q,
                    longitude,
                    latitude
                )
                expect(dbRepository.fetchCities.callCount).to.equal(1)
                expect(response.suggestions.length).to.equal(mockCities.length)
                for (const suggestion of response.suggestions) {
                    const found = isSuggesionCityInArrayOfCities(suggestion, mockCities)
                    const suggestionScore =
                        suggestion.stringSimilarity * SuggestionsService.NAME_PRIORITY_WEIGHT +
                        suggestion.distanceScore * SuggestionsService.DISTANCE_PRIORITY_WEIGHT

                    expect(found && suggestionScore === suggestion.score).to.equal(true)
                }
            })
        })
    })
})

function isSuggesionCityInArrayOfCities(suggestion: Suggestion, cities: City[]): boolean {
    return cities.some(
        (city: City) =>
            city.name === suggestion.name &&
            city.latitude === suggestion.latitude &&
            city.longitude === suggestion.longitude
    )
}
