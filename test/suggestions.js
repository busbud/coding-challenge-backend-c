var expect = require('chai').expect
const { createServer } = require('../dist/api/server.js')
const {TEST_MAX_TIME_PER_WINDOW, TEST_MAX_SERVER_REQUESTS_PER_WINDOW}  = require ('../src/constants.ts')
const app = createServer(
    { // limit IP to max # of requests per time window
        windowMs: TEST_MAX_TIME_PER_WINDOW, 
        max: TEST_MAX_SERVER_REQUESTS_PER_WINDOW,
        message:
            'Too many requests from this IP. Please try again later (test).',
        statusCode: 429,
    },
    false
)
var request = require('supertest')(app)

describe('GET /suggestions', function () {
    describe('with a non-existent city', function () {
        var response

        before(function (done) {
            request
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .end(function (err, res) {
                    response = res
                    response.json = JSON.parse(res.text)
                    done(err)
                })
        })

        it('returns a 404', function () {
            expect(response.statusCode).to.equal(404)
        })

        it('returns an empty array of suggestions', function () {
            expect(response.json.suggestions).to.be.instanceof(Array)
            expect(response.json.suggestions).to.have.length(0)
        })
    })

    describe('with a valid city', function () {
        var response

        before(function (done) {
            request.get('/suggestions?q=Montréal').end(function (err, res) {
                response = res
                response.json = JSON.parse(res.text)
                done(err)
            })
        })

        it('returns a 200', function () {
            expect(response.statusCode).to.equal(200)
        })

        it('returns an array of suggestions', function () {
            expect(response.json.suggestions).to.be.instanceof(Array)
            expect(response.json.suggestions).to.have.length.above(0)
        })

        describe('Validate the shape of the data being returned', function () {
            it('contains latitudes and longitudes', function () {
                expect(response.json.suggestions).to.satisfy(
                    function (suggestions) {
                        return suggestions.every(function (suggestion) {
                            return suggestion.latitude && suggestion.longitude
                        })
                    }
                )
            })

            it('contains scores', function () {
                expect(response.json.suggestions).to.satisfy(
                    function (suggestions) {
                        return suggestions.every(function (suggestion) {
                            return suggestion.score
                        })
                    }
                )
            })
        })
        it('contains a match', function () {
            expect(response.json.suggestions).to.satisfy(
                function (suggestions) {
                    return suggestions.some(function (suggestion) {
                        return /montreal/i.test(
                            suggestion.name
                                .normalize('NFD')
                                .replace(/[\u0300-\u036f]/g, '')
                        )
                    })
                }
            )
        })
    })
    describe('verify SQL injection is impossible', function () {
        var response

        before(function (done) {
            request
                .get('/suggestions?q=;%20DROP%20TABLE%20%22City%22;--')
                .end(function (err, res) {
                    response = res
                    response.json = JSON.parse(res.text)
                    done(err)
                })
        })

        it('finds no city named "; DROP TABLE "City";--"', function () {
            expect(response.statusCode).to.equal(404)
            expect(response.json.message).to.include('no data matches')
            expect(response.json.suggestions).to.have.length(0)
        })
    })
    describe('verify SQL injection changed nothing', function () {
        var response

        before(function (done) {
            request.get('/suggestions?q=Montréal').end(function (err, res) {
                response = res
                response.json = JSON.parse(res.text)
                done(err)
            })
        })

        it('still has entities in table "City" after SQL injection attempt', function () {
            expect(response.statusCode).to.equal(200)
            expect(response.json.suggestions).to.be.instanceof(Array)
            expect(response.json.suggestions).to.have.length.above(0)
        })
    })
    describe('verify correct latitude/longitude inputs', function () {
        var response

        before(function (done) {
            request
                .get('/suggestions?q=Lond&latitude=99&longitude=-40')
                .end(function (err, res) {
                    response = res
                    response.json = JSON.parse(res.text)
                    done(err)
                })
        })

        it('returns an invalid input error', function () {
            expect(response.statusCode).to.equal(404)
            expect(response.json.message).to.include(
                'Invalid latitude or longitude value.'
            )
        })
    })

    describe('verify that coordinates provide a more confident autocomplete suggestion', function () {
        var response
        before(function (done) {
            request.get('/suggestions?q=Lond').end(function (err, res) {
                response = res
                response.json = JSON.parse(res.text)
                done(err)
            })
        })

        var responseWithCoordinates
        before(function (done) {
            request
                .get('/suggestions?q=Lond&latitude=43.70011&longitude=-79.4163')
                .end(function (err, res) {
                    responseWithCoordinates = res
                    responseWithCoordinates.json = JSON.parse(res.text)
                    done(err)
                })
        })

        it('returns a 200', function () {
            expect(response.statusCode).to.equal(200)
            expect(responseWithCoordinates.statusCode).to.equal(200)
        })

        it('has a higher score when coordinates are provided', function () {
            expect(
                responseWithCoordinates.json.suggestions[0].score
            ).to.be.above(response.json.suggestions[0].score)
        })
    })
    describe('verify express-rate-limit', function () {
        var response

        before(function (done) {
            request.get('/suggestions?q=limit').end(function (err, res) {
                response = res
                done(err)
            })
        })
        it('returns too many requests error', function () {
            expect(response.status).to.equal(429)
            expect(response.text).to.include('Too many requests')
        })
    })
})
