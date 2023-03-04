/* eslint sonarjs/cognitive-complexity: 0 */
import { expect } from 'chai'
import { createSandbox, SinonSandbox, SinonStubbedInstance } from 'sinon'
import { App as TestApp } from '../../app/App'
import supertest from 'supertest'
import { CollectionReference, DocumentData, Firestore } from '@google-cloud/firestore'
import { kernel } from '../../app/Kernel'
import { TYPES } from '../../app/utils/Types'
import { City } from '../../app/entities/City'
import { fireStoreDbQueryResponse } from '../fixtures/FirestoreFetchCitiesReturns'
import { mockfetchSuggestionsReturns } from '../fixtures/SuggestionsServiceReturns'
import { SuggestionsService } from '../../app/services/SuggestionsService'

const sandbox: SinonSandbox = createSandbox()
let app: TestApp = null

let firestoreDb: SinonStubbedInstance<Firestore>
const mockFireStoreDBResponse: City[] = fireStoreDbQueryResponse['citiesResponse1']
let mockWhere: any
let mockGet: any
describe('Validate /suggestions endpoint', () => {
    beforeEach(async () => {
        sandbox.stub(kernel.get(TYPES.Redis))
        firestoreDb = sandbox.stub(kernel.get(TYPES.Firestore))

        mockGet = (): any => {
            return mockFireStoreDBResponse
        }
        mockWhere = (): any => {
            return {
                get: mockGet,
                where: mockWhere
            }
        }
        firestoreDb.collection.returns({
            where: mockWhere
        } as unknown as CollectionReference<DocumentData>)
        app = await new TestApp(12345).start()
    })
    afterEach(async () => {
        sandbox.restore()
        sandbox.resetHistory()
        await app.stop()
    })

    describe('validate the responses of GET /suggestions endpoint', () => {
        it('should respond with 400 when no query params are passed', async () => {
            const response = await supertest(app.getServer()).get('/suggestions').expect(400)
            expect(response.status).to.be.equal(400)
            expect(JSON.parse(response.text)).to.deep.equal({
                errors: [
                    {
                        instancePath: '',
                        keyword: 'required',
                        message: "must have required property 'q'",
                        params: {
                            missingProperty: 'q'
                        },
                        schemaPath: '#/required'
                    }
                ]
            })
        })
        it('should respond with 400 with missing incorrect format message if latitude is passed but as a string', async () => {
            const response = await supertest(app.getServer())
                .get('/suggestions?q=ut&latitude=u897')
                .expect(400)
            expect(response.status).to.be.equal(400)
            expect(JSON.parse(response.text)).to.deep.equal({
                errors: [
                    {
                        instancePath: '/latitude',
                        keyword: 'type',
                        message: 'must be number',
                        params: {
                            type: 'number'
                        },
                        schemaPath: '#/properties/latitude/type'
                    }
                ]
            })
        })
        it('should respond with 400 with missing incorrect format message if longitude is passed but as a string', async () => {
            const response = await supertest(app.getServer())
                .get('/suggestions?q=ut&longitude=u897')
                .expect(400)
            expect(response.status).to.be.equal(400)
            expect(JSON.parse(response.text)).to.deep.equal({
                errors: [
                    {
                        instancePath: '/longitude',
                        keyword: 'type',
                        message: 'must be number',
                        params: {
                            type: 'number'
                        },
                        schemaPath: '#/properties/longitude/type'
                    }
                ]
            })
        })
        it('should respond with 200 if q is passed but none of latitude or longitude', async () => {
            const response = await supertest(app.getServer()).get('/suggestions?q=jh').expect(200)
            expect(response.status).to.be.equal(200)
            const suggestionsGivenInResponse = JSON.parse(response.text)
            expect(suggestionsGivenInResponse).to.deep.equal(
                mockfetchSuggestionsReturns.suggestion1
            )
            for (const suggestion of suggestionsGivenInResponse.suggestions) {
                // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                const isScoreBasedOnStringSimilarityCompletely =
                    suggestion.score === suggestion.stringSimilarity
                expect(isScoreBasedOnStringSimilarityCompletely).to.equal(true)
            }
        })
        it('should respond with 200 if q is passed along with only longitude', async () => {
            const response = await supertest(app.getServer())
                .get('/suggestions?q=jh&longitude=11.34')
                .expect(200)
            expect(response.status).to.be.equal(200)
            const suggestionsGivenInResponse = JSON.parse(response.text)
            expect(suggestionsGivenInResponse).to.deep.equal(
                mockfetchSuggestionsReturns.suggestion1
            )
            for (const suggestion of suggestionsGivenInResponse.suggestions) {
                // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                const isScoreBasedOnStringSimilarityCompletely =
                    suggestion.score === suggestion.stringSimilarity
                expect(isScoreBasedOnStringSimilarityCompletely).to.equal(true)
            }
        })
        it('should respond with 200 if q is passed along with only latitude', async () => {
            const response = await supertest(app.getServer())
                .get('/suggestions?q=jh&latitude=-182.1')
                .expect(200)
            const suggestionsGivenInResponse = JSON.parse(response.text)
            expect(response.status).to.be.equal(200)
            expect(suggestionsGivenInResponse).to.deep.equal(
                mockfetchSuggestionsReturns.suggestion1
            )
            for (const suggestion of suggestionsGivenInResponse.suggestions) {
                // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                const isScoreBasedOnStringSimilarityCompletely =
                    suggestion.score === suggestion.stringSimilarity
                expect(isScoreBasedOnStringSimilarityCompletely).to.equal(true)
            }
        })
        it('should respond with 200 if q is passed along with both longitude and latitude', async () => {
            const response = await supertest(app.getServer())
                .get('/suggestions?q=mo&latitude=76.1&longitude=11.66')
                .expect(200)
            const suggestionsGivenInResponse = JSON.parse(response.text)
            expect(response.status).to.be.equal(200)
            for (const suggestion of suggestionsGivenInResponse.suggestions) {
                // if both latitude and longitude are not given then stringSimilarity score should be same as the final score for a suggestion
                const suggestionScore =
                    suggestion.stringSimilarity * SuggestionsService.NAME_PRIORITY_WEIGHT +
                    suggestion.distanceScore * SuggestionsService.DISTANCE_PRIORITY_WEIGHT

                const isScoreBasedOnStringSimilarityCompletely =
                    suggestion.score === suggestion.stringSimilarity
                expect(isScoreBasedOnStringSimilarityCompletely).to.equal(false)
                expect(suggestion.score).to.equal(suggestionScore)
            }
        })
    })
})
