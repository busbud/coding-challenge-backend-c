"use strict";
/**
 * Module
 * Handles parsing and constructing trie for city data search;
 */
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
}
Object.defineProperty(exports, "__esModule", { value: true });
const fs_1 = __importDefault(require("fs"));
const TrieSearch = require("trie-search");
const CITY_POPULATION_CUTOFF = 5000;
/**
 *
 * Get the tsv data if it doens't exist
 * @todo This would get the latest data zipped and unzip in data folder
 */
let getCityData = () => __awaiter(this, void 0, void 0, function* () {
    //superagent.get('http://download.geonames.org/export/dump/cities5000.zip')
});
exports.getCityData = getCityData;
/**
 * Stream TSV into dict
 * @param {string} dataPath
 * @returns {Promise<any>}
 */
let readAndParseCityDictFromGeoTsv = (dataPath) => __awaiter(this, void 0, void 0, function* () {
    let stream = fs_1.default.createReadStream(dataPath);
    return new Promise((resolve, reject) => {
        let lineNum = 0;
        let dict = {};
        stream.on('readable', () => {
            let chunk;
            // While we have data process it;
            while (null !== (chunk = stream.read())) {
                chunk.toString().split('\n').forEach((line) => {
                    // skip header;
                    if (lineNum > 0) {
                        // descruct data we need and add it to dict
                        let [, name, ascii, altNames, lat, long, , , country, , region, , , , pop] = line.split('\t');
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
                            };
                        }
                    }
                    lineNum++;
                });
            }
        })
            .on('end', () => resolve(dict))
            .on('error', (err) => {
            reject(err);
        });
    });
});
exports.readAndParseCityDictFromGeoTsv = readAndParseCityDictFromGeoTsv;
/**
 * Constuct trie from dict
 * @param {ParsedCityDict} cityDict
 * @returns {any}
 */
let makeSearchTrie = (cityDict) => {
    let trie = new TrieSearch();
    trie.addFromObject(cityDict);
    return trie;
};
exports.makeSearchTrie = makeSearchTrie;
/**
 * One shot function to read data from tsv and contstruct trie
 * @param {string} dataPath
 * @returns {TrieSearch}
 */
let getDataAndMakeTrie = (dataPath = './data/cities_canada-usa.tsv') => __awaiter(this, void 0, void 0, function* () {
    if (!fs_1.default.existsSync(dataPath))
        yield getCityData();
    let cityDict = yield readAndParseCityDictFromGeoTsv(dataPath);
    return makeSearchTrie(cityDict);
});
exports.getDataAndMakeTrie = getDataAndMakeTrie;
