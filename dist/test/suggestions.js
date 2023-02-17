"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const chai_1 = require("chai");
const app_1 = require("../app");
const supertest_1 = __importDefault(require("supertest"));
describe('GET /suggestions', function () {
    describe('with a non-existent city', function () {
        let response;
        before(function (done) {
            (0, supertest_1.default)(app_1.app)
                .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
                .end(function (err, res) {
                response = res;
                done(err);
            });
        });
        it('returns a 404', function () {
            (0, chai_1.expect)(response.statusCode).to.equal(404);
        });
        it('returns an empty array of suggestions', function () {
            (0, chai_1.expect)(response.body.suggestions).to.be.instanceof(Array);
            (0, chai_1.expect)(response.body.suggestions).to.have.length(0);
        });
    });
    describe('with a valid city', function () {
        let response;
        before(function (done) {
            (0, supertest_1.default)(app_1.app)
                .get('/suggestions?q=Montréal')
                .end(function (err, res) {
                response = res;
                done(err);
            });
        });
        it('returns a 200', function () {
            (0, chai_1.expect)(response.statusCode).to.equal(200);
        });
        it('returns an array of suggestions', function () {
            (0, chai_1.expect)(response.body.suggestions).to.be.instanceof(Array);
            (0, chai_1.expect)(response.body.suggestions).to.have.length.above(0);
        });
        describe('Validate the shape of the data being returned', function () {
            it('contains latitudes and longitudes', function () {
                (0, chai_1.expect)(response.body.suggestions).to.satisfy(function (suggestions) {
                    return suggestions.every(function (suggestion) {
                        return suggestion.latitude && suggestion.longitude;
                    });
                });
            });
            it('contains scores', function () {
                (0, chai_1.expect)(response.body.suggestions).to.satisfy(function (suggestions) {
                    return suggestions.every(function (suggestion) {
                        return suggestion.latitude && suggestion.longitude;
                    });
                });
            });
        });
        it('contains a match', function () {
            (0, chai_1.expect)(response.body.suggestions).to.satisfy(function (suggestions) {
                return suggestions.some(function (suggestion) {
                    return /Montréal/i.test(suggestion.name);
                });
            });
        });
    });
    // other test cases
});
//# sourceMappingURL=suggestions.js.map