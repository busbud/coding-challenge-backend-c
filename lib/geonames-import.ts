/**
 * Module
 * Handles parsing and constructing trie for city data search;
 */

import fs from 'fs';
import * as superagent from 'superagent';
import TrieSearch = require('trie-search');

const CITY_POPULATION_CUTOFF = 5000;

type CityTsvEntry = string; // tsv entry;
type CityName = string;

export interface ParsedCityData {
    name: CityName;
    altNames: string;
    lat: number;
    long: number;
    country: string;
    region: string;
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
 *
 * Get the tsv data if it doens't exist
 * @todo This would get the latest data zipped and unzip in data folder
 */
let getCityData = async () => {
    //superagent.get('http://download.geonames.org/export/dump/cities5000.zip')
}


/**
 * Stream TSV into dict
 * @param {string} dataPath
 * @returns {Promise<any>}
 */
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
                        let [, name, ascii, altNames, lat, long, , , country, ,region, , , , pop] = line.split('\t');

                        let population = parseInt(pop);
                        if (population > CITY_POPULATION_CUTOFF) {
                            dict[`${ascii}-${country}`] = {
                                name,
                                altNames,
                                lat: parseFloat(lat),
                                long: parseFloat(long),
                                country,
                                region,
                                population
                            }
                        }

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

/**
 * Constuct trie from dict
 * @param {ParsedCityDict} cityDict
 * @returns {any}
 */
let makeSearchTrie = (cityDict: ParsedCityDict) => {
    let trie = new TrieSearch();
    trie.addFromObject(cityDict);
    return trie;
}

/**
 * One shot function to read data from tsv and contstruct trie
 * @param {string} dataPath
 * @returns {TrieSearch}
 */
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