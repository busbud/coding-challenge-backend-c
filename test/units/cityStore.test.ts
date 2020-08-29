import { expect } from 'chai';
import CityStore from '../../src/services/search/CityStore'
import CityLoader from '../../src/services/loaders/CityLoader'
import City from '../../src/types/City';

describe('Pre process cities', () => {
    let cityStore: CityStore;

    before('First load the cities from tsv', (done) => {
        new CityLoader()
            .loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then((data: City[]) => {
                cityStore = new CityStore(data);
                done();
            })
            .catch(error => done(error));
    });

    it('should be restricted to cities in the USA and Canada', () => {
        cityStore.cities.every((value: City) => {
            expect(value.country).to.satisfy((country: string) => country === 'USA' || country === 'CA');
        })
    });

    it('should be restricted with a population above 5000 people', () => {
        cityStore.cities.every((value: City) => {
            expect(value.population).to.satisfy((population: number) => population > 5000);
        })
    });

});