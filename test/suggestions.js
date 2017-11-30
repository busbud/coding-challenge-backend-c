const expect = require('chai').expect;
const app = require('../app');
const request = require('supertest')(app);

describe('GET with bad request', () => {
    it('returns a 404 if bad url', done => {
        request
            .get('/foo')
            .end((err, res) => {
                expect(res.statusCode).to.equal(404);
                done();
            });
    });
    it('returns a 400 if no query', done => {
        request
            .get('/suggestions')
            .end((err, res) => {
                expect(res.statusCode).to.equal(400);
                done();
            });
    });
    it('returns a 400 if not enough chars', done => {
        request
            .get('/suggestions?q=a')
            .end((err, res) => {
                expect(res.statusCode).to.equal(400);
                done();
            });
    });
});

describe('GET /suggestions', () => {
    describe('with a non-existent city', () => {
        let response;
        before(done => {
            request
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .end((err, res) => {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('returns an empty array of suggestions', () => {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length(0);
        });
    });

    describe('with a valid city', () => {
        let response;

        before(done => {
            request
                .get('/suggestions?q=Montréal')
                .end((err, res) => {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('returns a 200', () => {
            expect(response.statusCode).to.equal(200);
        });

        it('returns an array of suggestions', () => {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length.above(0);
        });

        it('contains a match', () => {
            expect(response.json.suggestions).to.satisfy(suggestions => {
                return suggestions.some(suggestion => {
                    return /montréal/i.test(suggestion.name);
                });
            })
        });

        it('contains latitudes and longitudes', () => {
            expect(response.json.suggestions).to.satisfy(suggestions => {
                return suggestions.every(suggestion => {
                    return suggestion.latitude && suggestion.longitude;
                });
            })
        });

        it('contains scores', () => {
            expect(response.json.suggestions).to.satisfy(suggestions => {
                return suggestions.every(suggestion => {
                    return suggestion.latitude && suggestion.longitude;
                });
            })
        });
    });

    describe("with lat and long", () => {
        let responseWithoutLatLong;

        before(done => {
            request
                .get('/suggestions?q=paris')
                .end((err, res) => {
                    responseWithoutLatLong = res;
                    responseWithoutLatLong.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('change scores', (done) => {
            require('supertest')(app)
                .get('/suggestions?q=paris&latitude=38.50815&longitude=-84.1663')
                .end((err, res) => {
                    res.json = JSON.parse(res.text);
                    expect(responseWithoutLatLong.json.suggestions.length).to.equal(res.json.suggestions.length);
                    expect(responseWithoutLatLong.text).to.not.equal(res.text);
                    done(err);
                });
        });
    });
});
