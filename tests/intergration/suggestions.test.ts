import app from '../../src/app';
import { expect } from 'chai';
import supertest from 'supertest';

const request = supertest(app);

describe('GET /v1/suggestions', function () {
  describe('with a non-existent city', function () {
    let response;

    before(function (done) {
      request.get('/v1/suggestions?q=SomeRandomCityInTheMiddleOfNowhere').end(function (err, res) {
        response = res;
        response.json = JSON.parse(res.text);
        done(err);
      });
    });

    it('returns a 404', function () {
      expect(response.statusCode).to.equal(404);
    });

    it('returns an empty array of suggestions', function () {
      expect(response.json.suggestions).to.be.instanceof(Array);
      expect(response.json.suggestions).to.have.length(0);
    });
  });

  describe('with invalid params', () => {
    let response;
    describe('with invalid `q`', () => {
      before((done) => {
        request.get('/v1/suggestions').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
      });

      it('should return an error', () => {
        expect(response.statusCode).to.equal(400);
        expect(response.json).to.have.property('error');
        expect(response.json.error).to.equal('Missing required `q`');
      });
    });

    describe('with invalid `latitude`', () => {
      before((done) => {
        request.get('/v1/suggestions?q=montreal&longitude=45.05&latitude=qwerty').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
      });

      it('should return an error', () => {
        expect(response.statusCode).to.equal(400);
        expect(response.json).to.have.property('error');
        expect(response.json.error).to.equal('Invalid `latitude`');
      });
    });

    describe('with invalid `longitude`', () => {
      before((done) => {
        request.get('/v1/suggestions?q=montreal&longitude=qwerty&latitude=45.05').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
      });

      it('should return an error', () => {
        expect(response.statusCode).to.equal(400);
        expect(response.json).to.have.property('error');
        expect(response.json.error).to.equal('Invalid `longitude`');
      });
    });

    describe('when only latitude is given', () => {
      before((done) => {
        request.get('/v1/suggestions?q=montreal&latitude=45.05').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
      });

      it('should return an error', () => {
        expect(response.statusCode).to.equal(400);
        expect(response.json).to.have.property('error');
        expect(response.json.error).to.equal('Provide both `latitude` and `longitude`');
      });
    });

    describe('when only longitude is given', () => {
      before((done) => {
        request.get('/v1/suggestions?q=montreal&longitude=45.05').end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
      });

      it('should return an error', () => {
        expect(response.statusCode).to.equal(400);
        expect(response.json).to.have.property('error');
        expect(response.json.error).to.equal('Provide both `latitude` and `longitude`');
      });
    });
  });

  describe('with a valid city', function () {
    let response;

    before(function (done) {
      request.get('/v1/suggestions?q=Montreal').end(function (err, res) {
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
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy(function (suggestions) {
          return suggestions.every(function (suggestion) {
            return suggestion.score && suggestion.score > 0 && suggestion.score <= 1;
          });
        });
      });
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy(function (suggestions) {
        return suggestions.some(function (suggestion) {
          return /montreal/i.test(suggestion.name);
        });
      });
    });
  });
});
