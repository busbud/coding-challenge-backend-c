import express, {
  Express,
  RequestHandler,
  Request,
  Response,
  NextFunction
} from 'express'
import RateLimit, { Message } from 'express-rate-limit'
import { celebrate, Joi, errors } from 'celebrate'
import diacritics from 'diacritics'
import { getCities, getStates } from './data'
import { similarity, THRESHOLD } from './leven'
import { Suggestion } from './types'

export const init = async (): Promise<Express> => {
  const [cities, states] = await Promise.all([getCities(), getStates()])

  const app = express()

  // TODO: Replace in-memory store with Redis, Mongo, etc.
  app.use(new RateLimit({
    windowMs: 1000,
    max: 60,
    message: { message: 'Too Many Requests' } as Message
  }) as RequestHandler)

  app.get(
    '/suggestions',
    celebrate({
      query: Joi.object()
        .keys({
          q: Joi.string().required(),
          latitude: Joi.number()
            .min(-90)
            .max(90),
          longitude: Joi.number()
            .min(-180)
            .max(180)
        })
        .and('longitude', 'latitude') // if either is present, the other is required
    }),
    ({ query: { q /*, latitude, longitude */ } }, res) => {
      // TODO: Paginate results to reduce to result set
      const suggestions: Suggestion[] = []

      for (const city of cities) {
        const score = similarity(
          diacritics.remove(q).toLowerCase(),
          city.name.toLowerCase()
        )

        const state = states[`${city.country}.${city.admin1}`]

        // TODO: Accuracy threshold could be controled via a query param
        if (score > THRESHOLD) {
          suggestions.push({
            name: `${city.ascii}, ${state}, ${city.country}`,
            latitude: city.lat,
            longitude: city.long,
            score: +score.toFixed(2)
          })
        }
      }

      // FIXME: Account for latitude and longitude
      suggestions.sort((s1, s2) => s2.score - s1.score)

      res.json({
        suggestions
      })
    }
  )

  app.use(errors())

  app.use((req, res) => {
    res.status(404).json({ message: 'Not Found' })
  })

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
    console.error(err)
    res.status(500).json({ message: 'Server Error' })
  })

  return app
}
