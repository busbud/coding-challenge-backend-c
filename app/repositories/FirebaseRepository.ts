import { IDatabaseRepository } from './IDatabaseRepository'
import { inject, injectable } from 'inversify'
import { TYPES } from '../utils/Types'
import { Logger } from 'pino'
import { SuggestionsResponse } from '../entities/SuggestionsResponseSchema'
@injectable()
export class FirebaseRepository implements IDatabaseRepository {
    constructor(
        @inject(TYPES.Logger)
        private readonly logger: Logger
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
