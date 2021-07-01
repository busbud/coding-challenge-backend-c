const boot = require('../../src/boot')
const chai = require('chai')
chai.use(require('chai-json-schema'))

describe('Integrations tests', () => {
  let app = null

  before(async function () {
    app = await boot()
  })

  describe('Health tests', () => {
    const responseSchema = {
      type: 'object',
      properties: {
        searchEngine: {
          type: 'object',
          properties: {
            clusterName: {
              type: 'string'
            },
            status: {
              type: 'string'
            }
          }
        },
        cacheManager: {
          type: 'object',
          properties: {
            status: {
              type: 'string'
            }
          }
        }
      }
    }

    describe('GET /health', () => {
      let response

      before(async function () {
        response = await app.inject({
          method: 'GET',
          url: '/health'
        })
      })

      it('Should respect the format', function () {
        chai.expect(response.json()).to.be.jsonSchema(responseSchema)
      })
    })
  })

  describe('Suggestions routes', () => {
    const responseSchema = {
      type: 'object',
      properties: {
        suggestions: {
          type: 'array',
          items: {
            type: 'object',
            properties: {
              name: {
                type: 'string'
              },
              latitude: {
                type: 'number'
              },
              longitude: {
                type: 'number'
              },
              score: {
                type: 'number'
              }
            }
          }
        }
      }
    }

    describe('GET /suggestions', () => {
      describe('with a non-existent city', function () {
        let response

        before(async function () {
          response = await app.inject({
            method: 'GET',
            url: '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere'
          })
        })

        it('Should returns a 404', function () {
          chai.expect(response.statusCode).to.equal(404)
        })

        it('Should returns an empty array of suggestions', function () {
          chai.expect(response.json().suggestions).to.be.instanceof(Array)
          chai.expect(response.json().suggestions).to.have.length(0)
        })
      })
    })

    describe('with a valid city', function () {
      let response

      before(async function () {
        response = await app.inject({
          method: 'GET',
          url: '/suggestions?q=Montreal'
        })
      })

      it('Should returns a 200', function () {
        chai.expect(response.statusCode).to.equal(200)
      })

      it('Should returns an array of suggestions', function () {
        chai.expect(response.json().suggestions).to.be.instanceof(Array)
        chai.expect(response.json().suggestions).to.have.length.above(0)
      })

      it('Should contains a match', function () {
        chai.expect(response.json().suggestions)
          .to
          .satisfy(function (suggestions) {
            return suggestions.some(function (suggestion) {
              return /montreal|montréal/i.test(suggestion.name)
            })
          })
      })

      it('Should respect the format', function () {
        chai.expect(response.json()).to.be.jsonSchema(responseSchema)
      })
    })
  })
})
