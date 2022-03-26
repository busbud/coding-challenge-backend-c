const expect = require('chai').expect;
const app = require('../../app');
const request = require('supertest')(app);

describe('GET /suggestions', function () {
    describe('without search text parameter', function () {
        let response;

        before(function (done) {
            request.get('/suggestions').end(function (err, res) {
                response = res;
                done(err);
            });
        });

        after(function (done) {
            app.close();
            done();
        });

        it('returns a 400', function () {
            expect(response.statusCode).to.equal(400);
        });
    });

    describe('without search text parameter', function () {
        let response;

        before(function (done) {
            request
                .get('/suggestions?invalid=parameters')
                .end(function (err, res) {
                    response = res;
                    done(err);
                });
        });

        after(function (done) {
            app.close();
            done();
        });

        it('returns a 400', function () {
            expect(response.statusCode).to.equal(400);
        });
    });

    describe('with missing longitude search parameter', function () {
        let response;

        before(function (done) {
            request
                .get('/suggestions?latitude=124098')
                .end(function (err, res) {
                    response = res;
                    done(err);
                });
        });

        after(function (done) {
            app.close();
            done();
        });

        it('returns a 400', function () {
            expect(response.statusCode).to.equal(400);
        });
    });

    describe('with missing latitude search parameter', function () {
        let response;

        before(function (done) {
            request
                .get('/suggestions?longitude=124098')
                .end(function (err, res) {
                    response = res;
                    done(err);
                });
        });

        after(function (done) {
            app.close();
            done();
        });

        it('returns a 400', function () {
            expect(response.statusCode).to.equal(400);
        });
    });

    describe('with a non-existent city', function () {
        let response;

        before(function (done) {
            request
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .end(function (err, res) {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        after(function (done) {
            app.close();
            done();
        });

        it('returns a 404', function () {
            expect(response.statusCode).to.equal(404);
        });

        it('returns an empty array of suggestions', function () {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length(0);
        });
    });

    describe('with a valid city', function () {
        let response;

        before(function (done) {
            request.get('/suggestions?q=Montreal').end(function (err, res) {
                response = res;
                response.json = JSON.parse(res.text);
                done(err);
            });
        });

        it('returns a 200', function () {
            expect(response.statusCode).to.equal(200);
        });

        it('returns an array of suggestions', function () {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length.above(0);
        });

        describe('Validate the shape of the data being returned', function () {
            it('contains latitudes and longitudes', function () {
                expect(response.json.suggestions).to.satisfy(function (
                    suggestions
                ) {
                    return suggestions.every(function (suggestion) {
                        return suggestion.latitude && suggestion.longitude;
                    });
                });
            });

            it('contains scores', function () {
                expect(response.json.suggestions).to.satisfy(function (
                    suggestions
                ) {
                    return suggestions.every(function (suggestion) {
                        return suggestion.latitude && suggestion.longitude;
                    });
                });
            });
        });

        it('contains a match', function () {
            expect(response.json.suggestions).to.satisfy(function (
                suggestions
            ) {
                return suggestions.some(function (suggestion) {
                    return /Montréal/i.test(suggestion.name);
                });
            });
        });
    });
});
