import { IDatabaseRepository } from './IDatabaseRepository'
import { inject, injectable } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { Firestore } from '@google-cloud/firestore'
import { City } from '../entities/City'

@injectable()
export class FirestoreRepository implements IDatabaseRepository {
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger,
        @inject(TYPES.Firestore)
        private readonly firestoreDb: Firestore
    ) {}
    public async fetchCities(): Promise<City[]> {
        const cities: City[] = []
        const query = await this.firestoreDb
            .collection('cities')
            .where('population', '>', 5000)
            .where('country', 'in', ['USA', 'US', 'CA'])
            .get()

        query.forEach((doc) => {
            cities.push(doc.data() as City)
        })

        return cities
    }
}
