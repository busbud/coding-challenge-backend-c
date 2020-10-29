/* eslint-disable func-names, prefer-arrow-callback */

const { expect } = require('chai');
const proxyquire = require('proxyquire');
const sinon = require('sinon');

const services = require('../../../src/services');

describe('Test the suggestions controller', function () {
  describe('Test the getSuggestions function', function () {
    it('Must return a list of suggestions', function () {
      // Arrange
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

      const query = {
        q: 'Londo',
        latitude: '43.70011',
        longitude: '-79.4163',
      };

      const stub = sinon.stub(services, 'getSuggestionsService').returns(suggestions);

      const { getSuggestions } = proxyquire('../../../src/controllers', {
        '../services': {
          getSuggestionsService: stub,
        },
        url: () => ({
          parse: sinon.fake.returns(query),
        }),
      });

      const req = {
        url: '/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163',
        method: 'GET',
      };

      const res = {
        statusCode: 200,
        end: sinon.fake.returns(stub),
      };

      // Atc
      getSuggestions(req, res);

      // Asset
      expect(res.statusCode).to.equal(200);
      expect(res.end.firstCall.args[0]).to.eql(JSON.stringify({ suggestions }));
    });
  });
});
