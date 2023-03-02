import { Next, Response } from 'restify'
import { Controller, Get } from 'inversify-restify-utils'
import { injectable, inject, named } from 'inversify'
import { SuggestionsService } from '../services/SuggestionsService'
import { ServiceUnavailableError } from 'restify-errors'
import { TYPES } from '../utils/Types'
import { Request, ajv } from '../utils/Request'
import { v4 as uuidv4 } from 'uuid'
import { SuggestionsRequestSchema } from '../entities/SuggestionsRequestSchema'

@injectable()
@Controller('/')
export class SuggestionsController {
    constructor(
        @inject(TYPES.Service)
        @named(TYPES.SuggestionsService)
        private readonly suggestionsService: SuggestionsService
    ) {}

    @Get('/suggestions')
    public async suggestions(req: Request, res: Response, next: Next): Promise<void> {
        const requestId = uuidv4()
        try {
            const valid = ajv.validate(SuggestionsRequestSchema, req.query)
            if (!valid) {
                res.json(400, { errors: ajv.errors })
            } else {
                const response = await this.suggestionsService.fetchSuggestions(
                    requestId,
                    req.query.q,
                    req.query.longitude,
                    req.query.latitude
                )
                res.json(200, response)
            }
            return next()
        } catch (e) {
            req.logger.error({
                msg: e.message,
                requestId,
                tags: ['controller', 'suggestions', 'error'],
                stack: e.stack,
                error: e
            })
            return next(new ServiceUnavailableError(e))
        }
    }
}
