import { Response, Router } from 'express'
import { findCities } from '../findCities'
import { SuggestionRequest } from './server'

export const suggestionsRouter: Router = Router()

suggestionsRouter.get('/', async (req: SuggestionRequest, res: Response) =>
    findCities(req, res)
)
