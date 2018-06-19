/* global loggerForTests wait sinon expect mocks fixtures */
var expect = require('chai').expect;
const supertest = require('supertest');

const { Main } = require('../src');

// The tests does not have to be executed with real data, data has to be mocked TODO
// Tests made with this data in database
/*
{
  "name" : "Abbotsford",
  "latitude" : 49.05798,
  "longitude" : -122.25257,
  "population" : 5957659.0,
  "_id" : ObjectId("5b282573040862500a011fbd")
}
{
  "name" : "Montreal",
  "latitude" : 34.2,
  "longitude" : 99.3,
  "population" : 5957659.0,
  "_id" : ObjectId("5b285998040862500a011fbf")
}
*/
// TODO unit test for each file
describe('Test suite for functionnal test', () => {
  let request;

  describe('with a non-existent city', function () {

    beforeEach(async () => {
      const main = new Main();
      await main.init();
      const app = main.router.serviceDriver;
      request = supertest(app);

    });

    async function entry({ url, test }) {
      await request
        .get(url)
        .end(function (err, res) {
          test(res)
        });
    }
    it('returns a 404', async function () {
      function test(response) {
        expect(response.statusCode).to.equal(404);
      }
      entry({ url: '/unkownPath', test });
    });

    it('returns an empty array of suggestions', function () {
      function test(res) {
        const response = JSON.parse(res.text);
        expect(response.suggestions).to.be.instanceof(Array);
        expect(response.suggestions).to.have.length(0);
      }
      entry({ url: '/suggestions?q=SomeRandomCityInTheMiddleOfNowhere', test });
    });
  });

  describe('with a valid city', function () {
    beforeEach(async () => {
      const main = new Main();
      await main.init();
      const app = main.router.serviceDriver;
      request = supertest(app);
    });

    async function entry({ url, test }) {
      await request
        .get(url)
        .end(function (err, res) {
          test(res)
        });
    }
    it('returns a match with city only', async function () {
      function test(res) {
        const response = JSON.parse(res.text);
        expect(res.statusCode).to.equal(200);
        expect(response.suggestions.length).to.equal(1);
        expect(response.suggestions[0].name).to.equal('Montreal');
      }
      entry({ url: '/suggestions?q=Montreal', test });
    });

    it('returns a match with latitude and longitude', async function () {
      function test(res) {
        const response = JSON.parse(res.text);
        expect(res.statusCode).to.equal(200);
        expect(response.suggestions.length).to.equal(1);
        expect(response.suggestions[0].name).to.equal('Montreal');
      }
      entry({ url: '/suggestions?q=Montreal&latitude=34.2&longitude=99.3', test });
    });
  });
});
