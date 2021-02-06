const { expect } = require('chai');
const CachedLocationRepository = require('../../src/repository/cachedLocationRepository');

const LocationRepository = require('../../src/repository/locationRepository');

LocationRepository.getLocations = jest.fn(() => [{
    name: 'New London, WI, USA',
    latitude: '44.39276',
    longitude: '-88.73983',
    score: 1,
},
{
    name: 'London, 08, Canada',
    latitude: '42.98339',
    longitude: '-81.23304',
    score: 1,
}]);

describe('Validate if result is cached', () => {
    it('cached result returned', () => {
        const nonCachedResult = CachedLocationRepository.getLocations('London');
        expect(LocationRepository.getLocations.mock.calls.length).to.equal(1);

        LocationRepository.getLocations.mockClear();
        const cachedResult = CachedLocationRepository.getLocations('London');

        expect(LocationRepository.getLocations.mock.calls.length).to.equal(0);
        expect(cachedResult).to.deep.equal(nonCachedResult);
    });
});
