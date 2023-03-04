/* eslint sonarjs/cognitive-complexity: 0 */
import { expect } from 'chai'
import { createSandbox, SinonStubbedInstance } from 'sinon'
import { SuggestionsService } from '../../../app/services/SuggestionsService'
import { SuggestionsController } from '../../../app/controllers/SuggestionsController'
import { kernel } from '../../../app/Kernel'
import { mockfetchSuggestionsReturns } from '../../fixtures/SuggestionsServiceReturns'

const sandbox = createSandbox()
let suggestionController: SuggestionsController
let suggestionsService: SinonStubbedInstance<SuggestionsService>
const logger: any = {
    info: (): any => null,
    error: (): any => null
}
const req: any = {
    query: {},
    logger
}
const res: any = {
    json: Function
}
let next: any
let resJsonStub: any
let errorStub: any
describe('SuggestionsService', () => {
    beforeEach(() => {
        sandbox.restore()
        kernel.unbindAll()
        next = sandbox.stub()
        resJsonStub = sandbox.stub(res, 'json' as any)
        errorStub = sandbox.stub(req.logger, 'error' as any)
        suggestionsService = sandbox.createStubInstance(SuggestionsService)
        suggestionController = new SuggestionsController(suggestionsService)
    })
    describe('suggestions', () => {
        describe('when query params are missed or params in incorrect format are passed', () => {
            it('should respond with 400 if q is not passed with missing q property message', async () => {
                req.query = {}
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    400,
                    {
                        errors: [
                            {
                                instancePath: '',
                                keyword: 'required',
                                message: "must have required property 'q'",
                                params: {
                                    missingProperty: 'q'
                                },
                                schemaPath: '#/required'
                            }
                        ]
                    }
                ])
            })
            it('should respond with 400 if q is passed as an empty string with missing q property message', async () => {
                req.query = { q: ' ' }
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    400,
                    {
                        errors: [
                            {
                                instancePath: '/q',
                                keyword: 'minLength',
                                message: 'must NOT have fewer than 1 characters',
                                params: {
                                    limit: 1
                                },
                                schemaPath: '#/properties/q/allOf/1/minLength'
                            }
                        ]
                    }
                ])
            })
            it('should respond with 400 with missing incorrect format message if latitude is passed but as a string', async () => {
                req.query = {
                    q: 'tor',
                    latitude: 'incorrect latitude format'
                }
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    400,
                    {
                        errors: [
                            {
                                instancePath: '/latitude',
                                keyword: 'type',
                                message: 'must be number',
                                params: {
                                    type: 'number'
                                },
                                schemaPath: '#/properties/latitude/type'
                            }
                        ]
                    }
                ])
            })
            it('should respond with 400 with missing incorrect format message if longitude is passed but as a string', async () => {
                req.query = {
                    q: 'tor',
                    longitude: 'incorrect longitude format'
                }
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    400,
                    {
                        errors: [
                            {
                                instancePath: '/longitude',
                                keyword: 'type',
                                message: 'must be number',
                                params: {
                                    type: 'number'
                                },
                                schemaPath: '#/properties/longitude/type'
                            }
                        ]
                    }
                ])
            })
        })
        describe('when required query param q is passed', () => {
            it('should respond with 200 if q is passed but none of latitude or longitude', async () => {
                req.query = {
                    q: 'tor'
                }
                suggestionsService.fetchSuggestions.resolves(
                    mockfetchSuggestionsReturns.suggestion2
                )
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    200,
                    mockfetchSuggestionsReturns.suggestion2
                ])
            })
            it('should respond with 200 if q is passed along with only longitude', async () => {
                req.query = {
                    q: 'tor',
                    longitude: '-99.87'
                }
                suggestionsService.fetchSuggestions.resolves(
                    mockfetchSuggestionsReturns.suggestion2
                )
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    200,
                    mockfetchSuggestionsReturns.suggestion2
                ])
            })
            it('should respond with 200 if q is passed along with only latitude', async () => {
                req.query = {
                    q: 'tor',
                    latitude: 33.87
                }
                suggestionsService.fetchSuggestions.resolves(
                    mockfetchSuggestionsReturns.suggestion2
                )
                await suggestionController.suggestions(req, res, next)
                expect(resJsonStub.callCount).to.equal(1)
                expect(resJsonStub.lastCall.args).to.deep.equal([
                    200,
                    mockfetchSuggestionsReturns.suggestion2
                ])
            })
        })
        it('should log correct error if Service throws error', async () => {
            req.query = {
                q: 'tor'
            }
            const errorMessage = 'Error thrown by SuggestionService'
            suggestionsService.fetchSuggestions.rejects(new Error(errorMessage))
            await suggestionController.suggestions(req, res, next)
            expect(resJsonStub.callCount).to.equal(0)
            expect(next.callCount).to.equal(1)
            expect(next.lastCall.args[0].code).to.equal('ServiceUnavailable')
            expect(errorStub.callCount).to.equal(1)
            expect(errorStub.lastCall.args[0].msg).to.equal('Error thrown by SuggestionService')
        })
    })
})
