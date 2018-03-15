import * as chai from 'chai';
import fs from 'fs';

import {getCityData, makeTrie, ParsedCityDict, readAndParseTsv, TrieSearchResult} from '../lib/geonames-import'

const {expect} = chai;

describe('Parse TSV data into dict and tree', function () {

    let dataPath = './data/cities_canada-usa.tsv';

    it('Should be able to pipe that stream into a dict and build out index (Trie-search)', async (done) => {

        if (!fs.existsSync(dataPath))
            await getCityData();

        let ParseComplete = readAndParseTsv(dataPath);

        /// Wait for dict construction to copmlete then build radix-tree
        let cityDict: ParsedCityDict = await ParseComplete;
        expect(cityDict).to.be.an.instanceOf(Object);

        let trie = makeTrie(cityDict);

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