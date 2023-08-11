import express, { Response, Application, Router } from 'express'
import morgan from 'morgan'
import { suggestionsRouter } from './router'
import { RateLimitRequestHandler, rateLimit } from 'express-rate-limit'

export type SuggestionRequest = {
    query: { q: string; latitude?: number; longitude?: number }
}

export const createServer = (limiterOptions: object, morganBool: boolean) => {
    const app: Application = express()
    // mitigation to handle high levels of traffic
    const limiter: RateLimitRequestHandler = rateLimit(limiterOptions)
    app.use(limiter)
    const routes = Router()
    routes.use('/suggest', suggestionsRouter)
    app.use(express.json()).use(express.urlencoded({ extended: false }))

    if (morganBool) {
        app.use(morgan('tiny'))
    }

    app.get('/', (req: SuggestionRequest, res: Response) => {
        res.json({ message: 'Welcome, you made it!' })
    }).use('/suggestions', suggestionsRouter)

    return app
}
