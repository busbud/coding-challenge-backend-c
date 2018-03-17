import * as chai from 'chai';
import fs from 'fs';

import {
    getCityData,
    makeSearchTrie,
    ParsedCityDict,
    readAndParseCityDictFromGeoTsv,
    TrieSearchResult
} from '../lib/geonames-import'

const {expect} = chai;

describe('Parse TSV City Data', function () {

    let dataPath = './data/cities_canada-usa.tsv';
    let cityDict: ParsedCityDict;

    it('pipe that stream into a dict', async (done) => {

        if (!fs.existsSync(dataPath))
            await getCityData();

        let ParseComplete = readAndParseCityDictFromGeoTsv(dataPath);
        /// Wait for dict construction to copmlete then build radix-tree
        cityDict = await ParseComplete;
        expect(cityDict).to.be.an.instanceOf(Object);
        done();
    })

    it('build out index (Trie-search) from dict', async (done) => {
        let trie = makeSearchTrie(cityDict);

        // Run a couple of sanity tests and make sure data we need is there
        [
            {data: trie.get('Montreal'), expectedResult: true},
            {data: trie.get('SomeRandomCityInTheMiddleOfNowhere'), expectedResult: false}

        ].forEach(({data, expectedResult}: { data: TrieSearchResult[], expectedResult: boolean }) =>
            data.forEach(({value}) => expect(
                !!(value.name && Math.abs(value.lat) && Math.abs(value.long))
            ).to.equal(expectedResult))
        );

        done()

    });
});