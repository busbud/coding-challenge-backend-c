import { expect } from 'chai';
import { app } from '../app';
import supertest, { Response } from 'supertest';

 


describe('GET /suggestions', function () {


  describe('with a non-existent city', function () {
    let response: supertest.Response;

    before(function (done) {
      supertest(app)
        .get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end(function (err, res) {
          response = res;
          done(err);
        });
    });

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length(0);
    });
  });











  
  describe('with a valid city', function () {
    let response: Response;

    before(function (done) {
      supertest(app)
        .get('/suggestions?q=Montréal')
        .end(function (err, res) {
          response = res;
          done(err);
        });
    });

    it('returns a 200', function () {
      expect(response.statusCode).to.equal(200);
    });

    it('returns an array of suggestions', function () {
      expect(response.body.suggestions).to.be.instanceof(Array);
      expect(response.body.suggestions).to.have.length.above(0);
    });



    describe('Validate the shape of the data being returned', function () {
      it('contains latitudes and longitudes', function () {
        expect(response.body.suggestions).to.satisfy(function (suggestions: any[]) {
          return suggestions.every(function (suggestion: { latitude: any; longitude: any; }) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });

      it('contains scores', function () {
        expect(response.body.suggestions).to.satisfy(function (suggestions: any[]) {
          return suggestions.every(function (suggestion: { latitude: any; longitude: any; }) {
            return suggestion.latitude && suggestion.longitude;
          });
        })
      });
    });

    it('contains a match', function () {
      expect(response.body.suggestions).to.satisfy(function (suggestions: any[]) {
        return suggestions.some(function (suggestion: { name: string; }) {
          return /Montréal/i.test(suggestion.name);
        });
      })
    });
  });


  // other test cases
});
