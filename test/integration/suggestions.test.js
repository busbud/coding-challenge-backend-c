import { expect } from 'chai'
import supertest from 'supertest'
import app from '../../src/'
import { flushDB, importDB } from '../utils'

describe('GET /suggestions', () => {
  describe('with a non-existent city', () => {
    let response

    before((done) => {
      const request = supertest(app())
      request
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end((err, res) => {
          response = res
          response.json = JSON.parse(res.text)
          done(err)
        })
    })

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200)
    })

    it('returns an empty array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array)
      expect(response.json.suggestions).to.have.length(0)
    })
  })

  describe('with a valid city', () => {
    let response

    before((done) => {
      const request = supertest(app())
      request
        .get('/suggestions?q=Montreal')
        .end((err, res) => {
          response = res
          response.json = JSON.parse(res.text)
          done(err)
        })
    })

    it('returns a 200', () => {
      expect(response.statusCode).to.equal(200)
    })

    it('returns an array of suggestions', () => {
      expect(response.json.suggestions).to.be.instanceof(Array)
      expect(response.json.suggestions).to.have.length.above(0)
    })

    it('contains a match', () => {
      expect(response.json.suggestions).to.satisfy((suggestions) => {
        return suggestions.some((suggestion) => {
          return suggestion.name.match(/montr[ée]al/i)
        })
      })
    })

    it('contains latitudes and longitudes', () => {
      const validateLatitudeAndLongitude = ({latitude, longitude}) => {
        return latitude && longitude
      }

      expect(response.json.suggestions).to.satisfy((suggestions) => {
        return suggestions.every(validateLatitudeAndLongitude)
      })
    })

    it('contains scores between 0 and 1', () => {
      const validateScore = ({score}) => {
        return score >= 0 && score <= 1
      }

      expect(response.json.suggestions).to.satisfy((suggestions) => {
        return suggestions.every(validateScore)
      })
    })
  })
})

