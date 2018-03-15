"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const geonames_import_1 = require("./geonames-import");
class util {
    constructor() {
        this.getSuggestionsFromRequest = (req) => __awaiter(this, void 0, void 0, function* () {
            let { q, latitude, longitude } = req;
            if (!q || q.length < 3)
                return [];
            if (latitude && longitude) {
                latitude = (!isNaN(latitude) && latitude) || (latitude.length && parseFloat(latitude));
                longitude = (!isNaN(longitude) && longitude) || (longitude.length && parseFloat(longitude));
            }
            let trie = yield this._trie;
            let tr = trie.get(q);
            let unNormalized = tr.map(({ value }) => {
                let { name, lat, long, country, population } = value;
                return {
                    name: `${name}, ${country}`,
                    latitude: lat.toString(),
                    longitude: long.toString(),
                    score: this.calcCityScoreFromCord(value, { latitude, longitude })
                };
            });
            let output = unNormalized;
            return output;
        });
        this._trie = geonames_import_1.getDataAndMakeTrie();
    }
    calcCityScoreFromCord(cityData, cord2) {
        let { population, long, lat } = cityData;
        let { latitude, longitude } = cord2;
        let score = 0;
        // Note: This is not very accurate calculation, as it doesn't take into account the curvature of the earth
        // but for this context, it will suffice
        if (latitude && longitude) {
            score = Math.sqrt(Math.pow((longitude - long), 2) - Math.pow((latitude - lat), 2));
        }
        // Final score add emphasis on distance form long/lat and small boost for population
        return Math.pow((score + 1), 2) + (population / 10000000);
    }
}
exports.util = util;
