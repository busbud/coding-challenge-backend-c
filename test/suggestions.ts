import { Express } from 'express'
import { expect } from 'chai'
import supertest, { SuperTest, Test, Response } from 'supertest'
import { init } from '../src/app'
import { Suggestion } from '../src/types'

let app: Express, request: SuperTest<Test>

before(async () => {
  app = await init()
  request = supertest(app)
})

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    let res: Response

    before(async () => {
      res = await request.get(
        '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere'
      )
    })

    it('returns a 200', () => {
      expect(res.status).to.equal(200)
    })

    it('returns an empty array of suggestions', () => {
      expect(res.body.suggestions).to.be.an('array')
      expect(res.body.suggestions).to.have.length(0)
    })
  })

  describe('with a valid city', function () {
    let res: Response

    before(async () => {
      res = await request.get('/suggestions?q=Montreal')
    })

    it('returns a 200', () => {
      expect(res.status).to.equal(200)
    })

    it('returns an array of suggestions', () => {
      expect(res.body.suggestions).to.be.an('array')
      expect(res.body.suggestions).to.have.length.above(0)
    })

    it('contains a match', () => {
      expect(res.body.suggestions).to.satisfy((suggestions: Suggestion[]) => {
        return suggestions.some(({ name }) => /montreal/i.test(name))
      })
    })

    it('contains latitudes and longitudes', () => {
      expect(res.body.suggestions).to.satisfy((suggestions: Suggestion[]) => {
        return suggestions.every(
          ({ latitude, longitude }) => latitude && longitude
        )
      })
    })

    it('contains scores', () => {
      expect(res.body.suggestions).to.satisfy((suggestions: Suggestion[]) => {
        return suggestions.every(({ score }) => score)
      })
    })
  })
})
