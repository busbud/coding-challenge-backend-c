import { expect } from 'chai';
import CitySearchEngine from '../../src/services/search/CitySearchEngine'
import CityLoader from '../../src/services/loaders/CityLoader'
import { City, SuggestionResult } from '../../src/types/City';

describe('Pre process cities', () => {
    let cityStore: CitySearchEngine;

    before('load the cities from tsv', (done) => {
        new CityLoader()
            .loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => {
                cityStore = CitySearchEngine.instance;
                done();
            })
            .catch(error => done(error));
    });

    it('restricts to cities in the USA and Canada', () => {
        cityStore.cities.every((value: City) => {
            expect(value.country).to.satisfy((country: string) => country === 'USA' || country === 'CA');
        })
    });

    it('restricts with a population above 5000 people', () => {
        cityStore.cities.every((value: City) => {
            expect(value.population).to.satisfy((population: number) => population > 5000);
        })
    });


});

describe('Searching a match', () => {
    const cityEngine: CitySearchEngine = new CitySearchEngine();

    before('build the indexes', (done) => {
        cityEngine.initialize([{
            name: 'Florianópolis',
            latitude: -27.5853589,
            longitude: -48.5087219,
            country: 'BR',
            population: 477798,
            province: 'SC'
        },
        {
            name: 'Metropolis',
            latitude: 37.1611569,
            longitude: -88.7298181,
            country: 'US',
            population: 6537,
            province: 'IL'
        }]);
        done();
    });

    it('finds only Florianópolis', async () => {
        const result: SuggestionResult[] = await cityEngine.findBy({ q: 'Florianó' });
        expect(result.length).to.be.equals(1);
        expect(result[0].name).to.be.equals('Florianópolis, SC, BR');
        expect(result[0].score).to.be.greaterThan(0.3);
    })
});

