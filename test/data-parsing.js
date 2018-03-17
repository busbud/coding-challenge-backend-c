"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
}
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
}
Object.defineProperty(exports, "__esModule", { value: true });
const chai = __importStar(require("chai"));
const fs_1 = __importDefault(require("fs"));
const geonames_import_1 = require("../lib/geonames-import");
const { expect } = chai;
describe('Parse TSV City Data', function () {
    let dataPath = './data/cities_canada-usa.tsv';
    let cityDict;
    it('pipe that stream into a dict', (done) => __awaiter(this, void 0, void 0, function* () {
        if (!fs_1.default.existsSync(dataPath))
            yield geonames_import_1.getCityData();
        let ParseComplete = geonames_import_1.readAndParseCityDictFromGeoTsv(dataPath);
        /// Wait for dict construction to copmlete then build radix-tree
        cityDict = yield ParseComplete;
        expect(cityDict).to.be.an.instanceOf(Object);
        done();
    }));
    it('build out index (Trie-search) from dict', (done) => __awaiter(this, void 0, void 0, function* () {
        let trie = geonames_import_1.makeSearchTrie(cityDict);
        // Run a couple of sanity tests and make sure data we need is there
        [
            { data: trie.get('Montreal'), expectedResult: true },
            { data: trie.get('SomeRandomCityInTheMiddleOfNowhere'), expectedResult: false }
        ].forEach(({ data, expectedResult }) => data.forEach(({ value }) => expect(!!(value.name && Math.abs(value.lat) && Math.abs(value.long))).to.equal(expectedResult)));
        done();
    }));
});
