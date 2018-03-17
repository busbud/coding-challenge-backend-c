"use strict";
/**
 *  Class with utilities to take complexity away from app.js, handles throttling and orchestrating data
 */
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
const util_1 = require("util");
// Throttle connections from same IP by this amount of milliseconds
const CONNECTION_THROTTLE_TIMEOUT = 300;
class util {
    /**
     * constructor kicks off parsing of tsv data and stores the promise to fullfill
     */
    constructor() {
        this._connections = {};
        /**
         * Get suggestions based on request parameters
         * Uses the trie to find cities matching and then calculates and normailzes scores
         * @param requestParams
         * @returns {Promise<CitySuggestion[]>}
         */
        this.getSuggestionsFromRequest = (requestParams) => __awaiter(this, void 0, void 0, function* () {
            let { q, latitude, longitude } = requestParams;
            if (!q || q.length < 3)
                return [];
            // Long, lat are numbers or if string parse to number
            if (latitude && longitude)
                [latitude, longitude] = [latitude, longitude].map(x => (util_1.isNumber(x) && x) || (util_1.isString(x) && parseFloat(x)));
            // wait fot city data load and parse to fullfill
            let trie = yield this._trie;
            let tr = trie.get(q);
            let minScore, maxScore;
            // Generate output as required
            let output = tr
                .map(({ value }) => {
                let { name, lat, long, country, region } = value;
                // Generate non-normalized score for every city
                let score = this.calcCityScoreFromCord(value, { latitude, longitude });
                // Do min/max Score captures here for normailzation between 0,1
                if (minScore == null || score < minScore)
                    minScore = score;
                if (maxScore == null || score > maxScore)
                    maxScore = score;
                // Sample Output in repo had lat/long as string, so string here
                return {
                    name: `${name}, ${region}, ${country}`,
                    latitude: lat.toString(),
                    longitude: long.toString(),
                    score,
                };
                // Now all scores have been calculted we can normailze them to fit between 0,1 using min/max
            }).map(suggestion => (Object.assign({}, suggestion, { score: ((suggestion.score - minScore) / (maxScore - minScore)).toFixed(4) }))).sort((s1, s2) => s2.score - s1.score);
            // console.log(output);
            return output;
        });
        this._trie = geonames_import_1.getDataAndMakeTrie();
    }
    /**
     * Very basic throttling based on Promises and SetTimeout per IP
     * @todo This should be a queue per ip/user-agent , cookie etc that faucets allowances per timeslott
     * @param {"http".IncomingMessage} req
     * @returns {Promise<any>}
     */
    thottleConnection(req) {
        return __awaiter(this, void 0, void 0, function* () {
            // No unfullfilled connection for this IP , register one with 300 timeout
            if (!this._connections[req.connection.remoteAddress]) {
                this._connections[req.connection.remoteAddress] = new Promise((resolve, reject) => {
                    setTimeout(() => {
                        // Remove promise from this ip list and resolve
                        this._connections[req.connection.remoteAddress] = null;
                        resolve();
                    }, CONNECTION_THROTTLE_TIMEOUT);
                });
                // Return a true for first connection
                return Promise.resolve(true);
            }
            else {
                console.log('** Connection Throttle Activated ** ', req.connection.remoteAddress, 'waiting..\n');
                // Ip already has connection promise, wait for it to resolve
                return this._connections[req.connection.remoteAddress];
            }
        });
    }
    /**
     * Distance between two coordinates
     * Copied from http://snipplr.com/view/25479/
     */
    distance(lat1, lon1, lat2, lon2) {
        let R = 6371; // km (change this constant to get miles)
        let dLat = (lat2 - lat1) * Math.PI / 180;
        let dLon = (lon2 - lon1) * Math.PI / 180;
        let a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
            Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
                Math.sin(dLon / 2) * Math.sin(dLon / 2);
        let c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
        let d = R * c;
        return d;
    }
    /**
     * Calculate a non normalized score for city based on population and distance from supplied lat and long
     * @param {ParsedCityData} cityData
     * @param {Coordinate} cord2
     * @@todo Note this query requires context. For example: If the calculation is being done for a destination selection then really close distnaces should be penalized as people are unlikley to be booking a bus to go to another borrow.
     * @returns {any}
     */
    calcCityScoreFromCord(cityData, cord2) {
        let { population, long, lat, name, country } = cityData;
        let { latitude, longitude } = cord2;
        let score = 0;
        if (latitude && longitude) {
            // Larger distnace = smaller score
            score = 1e6 / (this.distance(lat, long, latitude, longitude) * 2);
        }
        // Final score add emphasis on distance form long/lat and small boost for population
        // i'm sure parameters can use some tuning, but the idea is there
        return score + ((population) ? score * (Math.log(population + 1)) : 0);
    }
}
exports.util = util;
