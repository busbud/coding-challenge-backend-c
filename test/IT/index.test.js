const supertest = require('supertest');
const { expect } = require('chai');

const App = require('../../app');

const request = supertest(new App().appListener);

describe('GET /suggestions', () => {
    describe('with a non-existent city', () => {
        let response;

        beforeAll(async () => {
            response = await request
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .send();
            response.json = JSON.parse(response.text);
        });

        it('returns a 404', () => {
            expect(response.statusCode).to.equal(404);
        });

        it('returns an empty array of suggestions', () => {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length(0);
        });
    });

    describe('with invalid params', () => {
        describe('with no query', () => {
            let response;

            beforeAll(async () => {
                response = await request
                    .get('/suggestions?q=')
                    .send();
                response.json = JSON.parse(response.text);
            });

            it('returns a 400', () => {
                expect(response.statusCode).to.equal(400);
            });

            it('returns an empty array of suggestions', () => {
                expect(response.json.code).to.equal(400);
                expect(response.json.message).to.equal('Query param cannot be empty.');
            });
        });

        describe('with latitude but no longitude', () => {
            let response;

            beforeAll(async () => {
                response = await request
                    .get('/suggestions?q=On&latitude=10.0')
                    .send();
                response.json = JSON.parse(response.text);
            });

            it('returns a 400', () => {
                expect(response.statusCode).to.equal(400);
            });

            it('returns an empty array of suggestions', () => {
                expect(response.json.code).to.equal(400);
                expect(response.json.message)
                    .to.equal('Both latitude and longitude values should be provided to use coordinates.');
            });
        });

        describe('with longitude but no latitude', () => {
            let response;

            beforeAll(async () => {
                response = await request
                    .get('/suggestions?q=On&longitude=10.0')
                    .send();
                response.json = JSON.parse(response.text);
            });

            it('returns a 400', () => {
                expect(response.statusCode).to.equal(400);
            });

            it('returns an empty array of suggestions', () => {
                expect(response.json.code).to.equal(400);
                expect(response.json.message)
                    .to.equal('Both latitude and longitude values should be provided to use coordinates.');
            });
        });

        describe('with invalid latitude', () => {
            describe('with latitude out of range (greater than 90)', () => {
                let response;

                beforeAll(async () => {
                    response = await request
                        .get('/suggestions?q=On&latitude=91&longitude=0')
                        .send();
                    response.json = JSON.parse(response.text);
                });

                it('returns a 400', () => {
                    expect(response.statusCode).to.equal(400);
                });

                it('returns an empty array of suggestions', () => {
                    expect(response.json.code).to.equal(400);
                    expect(response.json.message)
                        .to.equal('Latitude range from -90 to 90.');
                });
            });

            describe('with latitude out of range (less than -90)', () => {
                let response;

                beforeAll(async () => {
                    response = await request
                        .get('/suggestions?q=On&latitude=-91&longitude=0')
                        .send();
                    response.json = JSON.parse(response.text);
                });

                it('returns a 400', () => {
                    expect(response.statusCode).to.equal(400);
                });

                it('returns an empty array of suggestions', () => {
                    expect(response.json.code).to.equal(400);
                    expect(response.json.message)
                        .to.equal('Latitude range from -90 to 90.');
                });
            });
        });

        describe('with invalid longitude', () => {
            describe('with longitude out of range (greater than 180)', () => {
                let response;

                beforeAll(async () => {
                    response = await request
                        .get('/suggestions?q=On&latitude=0&longitude=181')
                        .send();
                    response.json = JSON.parse(response.text);
                });

                it('returns a 400', () => {
                    expect(response.statusCode).to.equal(400);
                });

                it('returns an empty array of suggestions', () => {
                    expect(response.json.code).to.equal(400);
                    expect(response.json.message)
                        .to.equal('Longitude range from -180 to 180.');
                });
            });

            describe('with latitude out of range (less than -180)', () => {
                let response;

                beforeAll(async () => {
                    response = await request
                        .get('/suggestions?q=On&latitude=0&longitude=-181')
                        .send();
                    response.json = JSON.parse(response.text);
                });

                it('returns a 400', () => {
                    expect(response.statusCode).to.equal(400);
                });

                it('returns an empty array of suggestions', () => {
                    expect(response.json.code).to.equal(400);
                    expect(response.json.message)
                        .to.equal('Longitude range from -180 to 180.');
                });
            });
        });
    });

    describe('with a valid city', () => {
        let notCachedLength = 0;
        let response;

        beforeAll(async () => {
            response = await request
                .get('/suggestions?q=Montreal')
                .send();
            response.json = JSON.parse(response.text);
        });

        it('returns a 200', () => {
            expect(response.statusCode).to.equal(200);
        });

        it('returns an array of suggestions', () => {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length.above(0);
            notCachedLength = response.json.suggestions.length;
        });

        describe('Validate the shape of the data being returned', () => {
            it('contains latitudes and longitudes', () => {
                expect(response.json.suggestions)
                    .to.satisfy((suggestions) => suggestions
                        .every((suggestion) => suggestion.latitude && suggestion.longitude));
            });

            it('contains scores', () => {
                expect(response.json.suggestions)
                    .to.satisfy((suggestions) => suggestions
                        .every((suggestion) => suggestion.latitude && suggestion.longitude));
            });
        });

        it('contains a match', () => {
            expect(response.json.suggestions)
                .to.satisfy((suggestions) => suggestions.some((suggestion) => suggestion.name.match(/montreal/i)));
        });

        describe('Validate that cached response is the same', () => {
            beforeAll(async () => {
                response = await request
                    .get('/suggestions?q=Montreal')
                    .send();
                response.json = JSON.parse(response.text);
            });

            it('returns a 200', () => {
                expect(response.statusCode).to.equal(200);
            });

            it('returns the same array of suggestions as before', () => {
                expect(response.json.suggestions).to.be.instanceof(Array);
                expect(response.json.suggestions.length).to.equal(notCachedLength);
            });
        });
    });
});
