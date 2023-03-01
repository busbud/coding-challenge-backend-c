import { inject, injectable, named } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { FirestoreRepository } from '../repositories/FirestoreRepository'
import { SuggestionsResponse } from '../entities/SuggestionsResponseSchema'

@injectable()
export class SuggestionsService {
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
        this.logger.info([
            {
                msg: `This is in service requestID: ${requestId}`,
                requestId,
                tags: ['service', 'suggestions', 'info']
            }
        ])
        this.db.fetchSuggestions(requestId, q, longitude, latitude)
        return {
            suggestions: []
        }
        // call DB to get data
    }
}
