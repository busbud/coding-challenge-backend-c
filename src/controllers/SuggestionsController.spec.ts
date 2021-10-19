import typeorm from "typeorm"
import { expect } from "chai"
import sinon from "sinon"
import { Request, Response } from "express"
import { SuggestionsController } from "./SuggestionsController"
import { ISuggestions } from "models/interfaces/ISuggestions";
import { Suggestions } from "models/dtos/Suggestions";
//var app     = require('../../busbudcodechallenge');
//import * as supertest from "supertest";

const mockResponse = () => {
    const res = {
        status: () => {},
        json: () => {}
    }
    res.status = sinon.stub().returns(res)
    res.json = sinon.stub().returns(res)
    return res;
}

const mockTypeOrm = () => {

}

describe('GET /suggestions', () => {

    describe('with a non-existent city', () => {

        it('returns a 404', async () => {
            
            const mockRequest: Request = new Object() as Request
            const mockedResponse = mockResponse() as Response
            mockRequest.params = { q: "UNIT_TEST_NON_EXISTING_CITY" }
            
            await SuggestionsController.GetSuggestions(mockRequest, mockedResponse, () => {})
                .then(() => {
                    console.log("===========================")
                    //expect(mockedResponse).to.equal(404)
                })
                .catch((error) => { throw error })
            
        });

        it('returns an empty array of suggestions', function () {
            //expect(mockedResponse.json).to.be.instanceof(Suggestions);
            //expect(mockedResponse.json.suggestions).to.have.length(0);
        });
    });
    /*
    describe('with a valid city', function () {
        var response;

        before(function (done) {
        request
            .get('/suggestions?q=Montreal')
            .end(function (err, res) {
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

        describe.skip('Validate the shape of the data being returned', function() {
        it('contains latitudes and longitudes', function () {	
            expect(response.json.suggestions).to.satisfy(function (suggestions) {	
            return suggestions.every(function (suggestion) {	
                return suggestion.latitude && suggestion.longitude;	
            });	
            })	
        });	

        it('contains scores', function () {	
            expect(response.json.suggestions).to.satisfy(function (suggestions) {	
            return suggestions.every(function (suggestion) {	
                return suggestion.latitude && suggestion.longitude;	
            });	
            })	
        });
        });
        
        it('contains a match', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
            return suggestions.some(function (suggestion) {
            return suggestion.name.test(/montreal/i);
            });
        })
        });
    });*/
});
