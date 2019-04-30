/* eslint-disable @typescript-eslint/explicit-function-return-type */
import * as chai from 'chai';
import * as sinon from 'sinon';

const {expect} = chai;
chai.use(require('sinon-chai'));
chai.use(require('chai-diff'));

import app from '../src/app';

import supertest from 'supertest';

const request = supertest(app);

interface SuggestionsResponse extends supertest.Response {
    json?: {[key: string]: object};
    statusCode?: number;
    suggestions?: object;
}

describe('GET /suggestions', function (): void {
    describe('with a non-existent city', function (): void {
        let response: SuggestionsResponse;

        before(function (done) {
            request
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .end(function (err, res) {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('returns a 404', function () {
            expect(response.statusCode).to.equal(404);
        });

        it('returns an empty array of suggestions', function () {
            let suggestions = null;
            suggestions = response && response.json && response.json.suggestions;
            expect(suggestions).to.be.instanceof(Array);
            expect(suggestions).to.have.length(0);
        });
    });

    describe('with a valid city', function (): void {
        let response: supertest.Response | any;

        before(function (done): void {
            request
                .get('/suggestions?q=Montreal')
                .end(function (err, res) {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('returns a 200', function (): void {
            expect(response.statusCode).to.equal(200);
        });

        it('returns an array of suggestions', function (): void {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length.above(0);
        });

        it('contains a match', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { some: (arg0: (suggestion: any) => any) => void }) {
                return suggestions.some(function (suggestion: { name: { match: (arg0: RegExp) => void } }) {
                    return suggestion.name.match(/montreal/i);
                });
            });
        });

        it('contains latitudes and longitudes', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { every: (arg0: (suggestion: any) => any) => void }) {
                return suggestions.every(function (suggestion: { latitude: any; longitude: any }) {
                    return suggestion.latitude && suggestion.longitude;
                });
            });
        });

        it('contains scores', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { every: (arg0: (suggestion: any) => boolean) => void }) {
                return suggestions.every(function (suggestion: { latitude: any; longitude: any }): boolean {
                    return suggestion.latitude && suggestion.longitude;
                });
            });
        });
    });

    describe('with a valid city and latitude longitude', function (): void {
        let response: supertest.Response | any;

        before(function (done): void {
            request
                .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
                .end(function (err, res) {
                    response = res;
                    response.json = JSON.parse(res.text);
                    done(err);
                });
        });

        it('returns a 200', function (): void {
            expect(response.statusCode).to.equal(200);
        });

        it('returns an array of suggestions', function (): void {
            expect(response.json.suggestions).to.be.instanceof(Array);
            expect(response.json.suggestions).to.have.length.above(0);
        });

        it('contains a match', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { some: (arg0: (suggestion: any) => any) => void }) {
                return suggestions.some(function (suggestion: { name: { match: (arg0: RegExp) => void } }) {
                    return suggestion.name.match(/London/i);
                });
            });
        });

        it('contains latitudes and longitudes', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { every: (arg0: (suggestion: any) => any) => void }) {
                return suggestions.every(function (suggestion: { latitude: any; longitude: any }) {
                    return suggestion.latitude && suggestion.longitude;
                });
            });
        });

        it('contains scores', function (): void {
            expect(response.json.suggestions).to.satisfy(function (suggestions: { every: (arg0: (suggestion: any) => boolean) => void }) {
                return suggestions.every(function (suggestion: { latitude: any; longitude: any }): boolean {
                    return suggestion.latitude && suggestion.longitude;
                });
            });
        });
    });
});
