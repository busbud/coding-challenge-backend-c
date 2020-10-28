/* eslint-disable func-names, prefer-arrow-callback */

const { expect } = require('chai');
const nock = require('nock');
const supertest = require('supertest');

const app = require('../../src/app');

const request = supertest(app);

describe('GET /suggestions', function () {
  // Test domain to run exclusively the integration tests.
  // The port number is exported by the `test:integration` script.
  const testDomain = 'http://127.0.0.1:5432';

  after(function (done) {
    app.close(done);
  });

  describe('with a non-existent city', function () {
    let response;

    before(function (done) {
      request.get('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere').end((err, res) => {
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

  describe('with a non-existent method', function () {
    let response;

    before(function (done) {
      request
        .post('/suggestions?q=SomeRandomCityInTheMiddleOfNowhere')
        .end((err, res) => {
          response = res;
          response.json = JSON.parse(res.text);
          done(err);
        });
    });

    it('returns a 400', function () {
      expect(response.statusCode).to.equal(400);
    });

    it('returns an object with a message property', function () {
      expect(response.json).to.be.an('object');
      expect(response.json).to.have.property('message');
    });
  });

  describe('with a valid city', function () {
    let response;

    before(function (done) {
      const suggestions = [
        {
          name: 'Montreal Bay',
          latitude: '54.9169',
          longitude: '-66.48183',
          score: 1,
        },
      ];

      nock(testDomain)
        .get('/suggestions?q=Montreal')
        .reply(200, { suggestions });

      request.get('/suggestions?q=Montreal').end((err, res) => {
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
      before(function (done) {
        const suggestions = [
          {
            name: 'Montreal Bay',
            latitude: '54.9169',
            longitude: '-66.48183',
            score: 1,
          },
          {
            name: 'London, ON, Canada',
            latitude: '42.98339',
            longitude: '-81.23304',
            score: 0.9,
          },
          {
            name: 'London, OH, USA',
            latitude: '39.88645',
            longitude: '-83.44825',
            score: 0.5,
          },
          {
            name: 'London, KY, USA',
            latitude: '37.12898',
            longitude: '-84.08326',
            score: 0.5,
          },
          {
            name: 'Londontowne, MD, USA',
            latitude: '38.93345',
            longitude: '-76.54941',
            score: 0.3,
          },
        ];

        nock(testDomain)
          .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
          .reply(200, { suggestions });

        request
          .get('/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163')
          .end((err, res) => {
            response = res;
            response.json = JSON.parse(res.text);
            done(err);
          });
      });

      it('contains latitudes and longitudes', function () {
        expect(response.json.suggestions).to.satisfy((suggestions) => {
          return suggestions.every((suggestion) => {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });

      it('contains scores', function () {
        expect(response.json.suggestions).to.satisfy((suggestions) => {
          return suggestions.every((suggestion) => {
            return suggestion.latitude && suggestion.longitude;
          });
        });
      });
    });

    it('contains a match', function () {
      expect(response.json.suggestions).to.satisfy((suggestions) => {
        return suggestions.some((suggestion) => {
          return suggestion.name.match(/montreal/i);
        });
      });
    });
  });
});
