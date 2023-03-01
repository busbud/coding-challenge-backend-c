import { IDatabaseRepository } from './IDatabaseRepository'
import { inject, injectable } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { SuggestionsResponse } from '../entities/SuggestionsResponseSchema'
import { Firestore } from '@google-cloud/firestore'
@injectable()
export class FirestoreRepository implements IDatabaseRepository {
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger,
        @inject(TYPES.Firestore)
        private readonly firestoreDb: Firestore
    ) {}
    public async fetchSuggestions(
        requestId: string,
        q: string,
        longitude?: number,
        latitude?: number
    ): Promise<SuggestionsResponse> {
        const loggerOutput = {
            msg: 'Inside Firebase Repository',
            requestId,
            tags: ['dbRepository', 'suggestions', 'info'],
            q,
            longitude,
            latitude
        }
        this.logger.info([loggerOutput])
        return {
            suggestions: []
        }
    }
}
