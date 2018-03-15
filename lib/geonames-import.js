"use strict";
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
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
}
Object.defineProperty(exports, "__esModule", { value: true });
const fs_1 = __importDefault(require("fs"));
const superagent = __importStar(require("superagent"));
const TrieSearch = require("trie-search");
/**
 * FIXME flesh t his out later on
 * Get the tsv data if it doens't exist
 */
let getCityData = () => __awaiter(this, void 0, void 0, function* () {
    superagent.get();
});
exports.getCityData = getCityData;
let readAndParseTsv = (dataPath) => __awaiter(this, void 0, void 0, function* () {
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
                        let [, name, ascii, altNames, lat, long, , country] = line.split('\t');
                        dict[ascii] = { name, altNames, lat: parseFloat(lat), long: parseFloat(long), country };
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
exports.readAndParseTsv = readAndParseTsv;
let makeTrie = (cityDict) => {
    let trie = new TrieSearch();
    trie.addFromObject(cityDict);
    return trie;
};
exports.makeTrie = makeTrie;
