import { expect } from 'chai'
import Koa from 'koa'
import supertest from 'supertest'
import error from '../../src/lib/error'

describe('Error middleware', () => {
  describe('with a non-existent route', () => {
    let response

    before((done) => {
      const app = new Koa()
      const request = supertest(app.listen())

      app.use(error())
      request
        .get('/badpath')
        .end((err, res) => {
          response = res
          done(err)
        })
    })

    it('returns a 404', () => {
      expect(response.statusCode).to.be.equal(404)
      expect(response.notFound).to.be.equal(true)
    })

    it('returns application/json', () => {
      expect(response.type).to.be.equal('application/json')
    })

    it('returns a not found message', () => {
      expect(response.body.error).to.be.equal('Not Found')
    })
  })
})
