const chai = require('chai');
const expect = chai.expect;
chai.use(require('chai-as-promised'));
chai.use(require('sinon-chai'));

const sinon = require('sinon');
const CityService = require('../lib/city.service');
const ScoreService = require('../lib/score.service');

describe('CityService', () => {
  let cityService;
  let mongoClient;
  let scoreService;
  let aggregate;

  before(() => {
    scoreService = sinon.createStubInstance(ScoreService);

    aggregate = sinon.stub();
    find = sinon.stub();

    mongoClient = {
      db: () => ({
        collection: () => ({
          aggregate,
          find
        })
      })
    };

    cityService = new CityService(mongoClient, scoreService);
  });

  describe('findCities function', () => {
    it('invoke mongo with a geonear aggregration if coords are given', () => {
      aggregate.returns({ toArray: () => [] });

      return expect(
        cityService.findCities({
          q: 'Montréal',
          latitude: 45.50884,
          longitude: -73.58781
        })
      ).to.be.fulfilled.then(r => {
        return expect(aggregate).to.have.been.calledWith([
          {
            $geoNear: {
              distanceField: 'distance',
              near: { coordinates: [-73.58781, 45.50884], type: 'Point' },
              query: { ascii: { $options: 'si', $regex: '^Montreal' } },
              spherical: true
            }
          },
          { $limit: 20 }
        ]);
      });
    });

    it('invoke mongo with a find query if coords are not given', () => {
      find.returns({
        limit: () => ({
          toArray: () => [
            {
              name: 'Ajax',
              ascii: 'Ajax',
              lat: 43.85012,
              long: -79.03288,
              country: 'CA',
              geometry: {
                type: 'Point',
                coordinates: [-79.03288, 43.85012]
              }
            }
          ]
        })
      });

      scoreService.score.returns(1);

      return expect(
        cityService.findCities({
          q: 'Montréal'
        })
      ).to.be.fulfilled.then(r => {
        expect(find).to.have.been.calledWith({
          ascii: { $options: 'si', $regex: '^Montreal' }
        });

        return expect(r[0]).to.deep.equal({
          latitude: 43.85012,
          longitude: -79.03288,
          name: 'Ajax, CA',
          score: 1
        });
      });
    });
  });
});
