import { inject, injectable, named } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { FirebaseRepository } from '../repositories/FirebaseRepository'
import { SuggestionsResponse } from '../entities/SuggestionsResponseSchema'

@injectable()
export class SuggestionsService {
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger,
        @inject(TYPES.IDatabaseRepository)
        @named(TYPES.FirebaseRepository)
        private readonly db: FirebaseRepository
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
