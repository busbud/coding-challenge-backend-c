import fs from 'fs';
import * as superagent from 'superagent';
import TrieSearch = require('trie-search');

type CityTsvEntry = string; // tsv entry;
type CityName = string;

export interface ParsedCityData {
    name: CityName;
    altNames: string;
    lat: number;
    long: number;
    country: string;
    population: number;
}

export interface ParsedCityDict {
    [cityName: string]: ParsedCityData;
}

export interface TrieSearchResult {
    _key_: CityName;
    value: ParsedCityData;
}

/**
 * FIXME flesh t his out later on
 * Get the tsv data if it doens't exist
 */
let getCityData = async () => {
    superagent.get()
}


let readAndParseCityDictFromGeoTsv = async (dataPath: string) => {
    let stream = fs.createReadStream(dataPath);
    return new Promise((resolve, reject) => {

        let lineNum = 0;
        let dict: ParsedCityDict = {};

        stream.on('readable', () => {
            let chunk;
            // While we have data process it;
            while (null !== (chunk = stream.read())) {
                chunk.toString().split('\n').forEach((line: CityTsvEntry) => {
                    // skip header;
                    if (lineNum > 0) {
                        // descruct data we need and add it to dict
                        let [, name, ascii, altNames, lat, long,,,country,,,,,,population] = line.split('\t');
                        dict[ascii] = {name, altNames, lat: parseFloat(lat), long: parseFloat(long), country,population}

                    }
                    lineNum++;
                });
            }
        })
            .on('end', () => resolve(dict))
            .on('error', (err) => {
                reject(err);
            })

    });
}

let makeSearchTrie = (cityDict: ParsedCityDict) => {
    let trie = new TrieSearch();
    trie.addFromObject(cityDict);
    return trie;
}

let getDataAndMakeTrie = async (dataPath = './data/cities_canada-usa.tsv'): TrieSearch => {
    if (!fs.existsSync(dataPath))
        await getCityData();

    let cityDict: ParsedCityDict = await readAndParseCityDictFromGeoTsv(dataPath);

    return makeSearchTrie(cityDict);
}

export {
    getCityData,
    readAndParseCityDictFromGeoTsv,
    makeSearchTrie,
    getDataAndMakeTrie
}