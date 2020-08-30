import { expect } from 'chai';
import CitySearchEngine from '../../src/services/search/CitySearchEngine'
import CityLoader from '../../src/services/loaders/CityLoader'
import { City, SuggestionResult, RequestSearchParam } from '../../src/types/City';

describe('Pre process cities', () => {
    let cityEngine: CitySearchEngine;

    before('load the cities from tsv', (done) => {
        new CityLoader()
            .loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => {
                cityEngine = CitySearchEngine.instance;
                done();
            })
            .catch(error => done(error));
    });

    it('restricts to cities in the USA and Canada', () => {
        cityEngine.cities.every((value: City) => {
            expect(value.country).to.satisfy((country: string) => country === 'USA' || country === 'CA');
        })
    });

    it('restricts with a population above 5000 people', () => {
        cityEngine.cities.every((value: City) => {
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

describe('More accuracy with latitude and longitude', () => {
    let cityEngine: CitySearchEngine;

    before('load the cities from tsv', (done) => {

        new CityLoader()
            .loadCitiesFromTsv('data/cities_canada-usa.tsv')
            .then(() => {
                cityEngine = CitySearchEngine.instance;
                done();
            })
            .catch(error => done(error));
    });

    it('returns London Ontario with score higher', async () => {
        const params: RequestSearchParam = {
            q: 'Londo',
            latitude: '43.70011',
            longitude: '-79.4163'
        }
        const result: SuggestionResult[] = await cityEngine.findBy(params);
        expect(result[0].name).to.be.equals('London, Ontario, CA');
    });
})

