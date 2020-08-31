import { expect } from 'chai';
import { describe } from 'mocha';
import { FuzzyVector, SearchCityFuzzy } from '../../src/types/Fuzzy';
import { City } from '../../src/types/City';
import FuzzyResolver from '../../src/services/search/FuzzyResolver';
import FuzzyCityIndexer from '../../src/services/search/FuzzyCityIndexer';

describe('Counting datagram', () => {
    it('creates a trigram of the word into a vector', () => {
        const vector = FuzzyResolver.getTrigram('banana');
        expect(vector[0]).to.be.eql({ nGram: '-ba', count: 1 });
        expect(vector[1]).to.be.eql({ nGram: 'ban', count: 1 });
        expect(vector[2]).to.be.eql({ nGram: 'ana', count: 2 });
        expect(vector[3]).to.be.eql({ nGram: 'nan', count: 1 });
        expect(vector[4]).to.be.eql({ nGram: 'na-', count: 1 });
    });
});

describe('Calculating vectors', () => {
    it('returns banana magnitude 2.828', () => {
        const vector: FuzzyVector[] = FuzzyResolver.getTrigram('banana');
        const result = FuzzyResolver.calculateMagnitude(vector);
        expect(result).to.be.closeTo(2.828, 0.001);
    });

    it('calculates the cosine similarity score', () => {
        const bananaGram: FuzzyVector[] = FuzzyResolver.getTrigram('banana');
        const searchCity: SearchCityFuzzy = {
            searchGram: FuzzyResolver.convertVectorToMap(bananaGram),
            magnitude: FuzzyResolver.calculateMagnitude(bananaGram),
        };

        const city: City = {
            name: 'Banan',
            latitude: -27.5853589,
            longitude: -48.5087219,
            country: 'XX',
            population: 5000,
            province: 'XX',
            nGram: FuzzyResolver.getTrigram('banan'),
        };
        city.magnitude = FuzzyResolver.calculateMagnitude(city.nGram);

        const score = FuzzyResolver.calculateCosineSimilarity(searchCity, city);

        expect(score).to.be.closeTo(0.791, 0.001);
    });
});

describe('Indexing the cities', () => {
    let cities: City[];
    let fuzzyBuilder: FuzzyCityIndexer;
    let florianopolis;
    let metropolis;

    before((done) => {
        cities = [{
            name: 'Florianópolis',
            latitude: -27.5853589,
            longitude: -48.5087219,
            country: 'BR',
            population: 477798,
            province: 'SC',
        },
        {
            name: 'Metropolis',
            latitude: 37.1611569,
            longitude: -88.7298181,
            country: 'US',
            population: 6537,
            province: 'IL',
        }];
        [florianopolis, metropolis] = cities;
        fuzzyBuilder = new FuzzyCityIndexer(cities);
        fuzzyBuilder.buildIndexes();
        done();
    });

    it('creates a index for the city Florianópolis', () => {
        expect(florianopolis.nGram).to.be.instanceOf(Array);
        expect(florianopolis.nGram).to.be.have.length(13);
        expect(florianopolis.magnitude).to.be.not.undefined;
    });

    it('creates a index for the city Metropolis', () => {
        expect(metropolis.nGram).to.be.instanceOf(Array);
        expect(metropolis.nGram).to.have.length(10);
        expect(metropolis.magnitude).to.be.not.undefined;
    });

    it('creates a dictionary with 18 elements', () => {
        expect(fuzzyBuilder.dictionary.size).to.be.equal(18);
    });

    it('creates the group of index by a gram into dictionary', () => {
        expect(fuzzyBuilder.dictionary.get('pol').length).to.be.equal(2);
        expect(fuzzyBuilder.dictionary.get('oli').length).to.be.equal(2);
        expect(fuzzyBuilder.dictionary.get('lis').length).to.be.equal(2);
        expect(fuzzyBuilder.dictionary.get('is-').length).to.be.equal(2);
    });

    it('finds only Florianópolis with similar gram from the word Florinda', () => {
        const citiesFiltered: City[] = fuzzyBuilder.findCityByGram(FuzzyResolver.getTrigram('Florinda'));

        expect(citiesFiltered.length).to.be.equals(1);
        expect(citiesFiltered[0].name).to.equal('Florianópolis');
    });
});
