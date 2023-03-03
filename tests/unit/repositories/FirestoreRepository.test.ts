import { expect } from 'chai'
import { createSandbox, SinonStubbedInstance } from 'sinon'
import { FirestoreRepository } from '../../../app/repositories/FirestoreRepository'
import { kernel } from '../../../app/Kernel'
import { Firestore, CollectionReference, DocumentData } from '@google-cloud/firestore'
import { Logger } from 'pino'
import { fireStoreDbQueryResponse } from '../../fixtures/FirestoreFetchCitiesReturns'
import { City } from '../../../app/entities/City'

const sandbox = createSandbox()
let firestoreRepositoryTest: FirestoreRepository
let firestoreDb: SinonStubbedInstance<Firestore>
let logger: SinonStubbedInstance<Logger>
let mockCitiesResponse: City[] = fireStoreDbQueryResponse['citiesResponse1']

describe('FirestoreRepository', () => {
    beforeEach(() => {
        sandbox.restore()
        kernel.unbindAll()

        firestoreDb = sandbox.createStubInstance(Firestore)
        const mockGet = (): any => {
            return mockCitiesResponse
        }
        const mockWhere = (): any => {
            return {
                get: mockGet,
                where: mockWhere
            }
        }
        firestoreDb.collection.returns({
            where: mockWhere
        } as unknown as CollectionReference<DocumentData>)
        firestoreRepositoryTest = new FirestoreRepository(logger, firestoreDb)
    })
    describe('fetchCities', () => {
        describe('should successfully call Firestore DB with correct where conditions', async () => {
            it('should return an array of length > 1 when query returns some items', async () => {
                const response = await firestoreRepositoryTest.fetchCities()
                expect(firestoreDb.collection.callCount).to.equal(1)
                expect(response.length).to.equal(fireStoreDbQueryResponse.citiesResponse1.length)
            })
            it('should return an empty array when query returns no items', async () => {
                mockCitiesResponse = [] as City[]
                const response = await firestoreRepositoryTest.fetchCities()
                expect(firestoreDb.collection.callCount).to.equal(1)
                expect(response.length).to.equal(0)
            })
        })
    })
})
