import { Router } from 'express'
import { findCities } from './handlers/findCities'

export const suggestionsRouter: Router = Router()

suggestionsRouter.get('/', findCities)
