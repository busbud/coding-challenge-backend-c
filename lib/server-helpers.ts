/**
 *  Class with utilities to take complexity away from app.js, handles throttling and orchestrating data
 */

import {getDataAndMakeTrie, ParsedCityData, TrieSearchResult} from './geonames-import';
// For type
import TrieSearch = require('trie-search');
import {IncomingMessage} from "http";
import {isNumber, isString} from "util";


type Coordinate = {
    latitude: number;
    longitude: number;
}

interface CitySuggestion {
    name: string;
    latitude: string;
    longitude: string;
    score: number;

}

// Throttle connections from same IP by this amount of milliseconds
const CONNECTION_THROTTLE_TIMEOUT = 300;

export class util {

    private _trie: Promise<TrieSearch>
    private _connections = {};

    /**
     * constructor kicks off parsing of tsv data and stores the promise to fullfill
     */
    constructor() {
        this._trie = getDataAndMakeTrie();
    }

    /**
     * Very basic throttling based on Promises and SetTimeout per IP
     * @todo This should be a queue per ip/user-agent , cookie etc that faucets allowances per timeslott
     * @param {"http".IncomingMessage} req
     * @returns {Promise<any>}
     */
    public async thottleConnection(req: IncomingMessage) {
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
    }

    /**
     * Distance between two coordinates
     * Copied from http://snipplr.com/view/25479/
     */
    private distance(lat1, lon1, lat2, lon2): number {
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
    public calcCityScoreFromCord(cityData: ParsedCityData, cord2: Coordinate): number {
        let {population, long, lat, name,country} = cityData;
        let {latitude, longitude} = cord2;
        let score = 0;
        if (latitude && longitude) {
            // Larger distnace = smaller score
            score = 1e6 / (this.distance(lat, long, latitude, longitude)*2)
        }
        // Final score add emphasis on distance form long/lat and small boost for population
        // i'm sure parameters can use some tuning, but the idea is there
        return score + ((population) ? score*(Math.log(population + 1)) : 0)
    }


    /**
     * Get suggestions based on request parameters
     * Uses the trie to find cities matching and then calculates and normailzes scores
     * @param requestParams
     * @returns {Promise<CitySuggestion[]>}
     */
    public getSuggestionsFromRequest = async (requestParams: any): Promise<CitySuggestion[]> => {
        let {q, latitude, longitude} = requestParams;

        if (!q || q.length < 3)
            return [];

        // Long, lat are numbers or if string parse to number
        if (latitude && longitude)
            [latitude, longitude] = [latitude, longitude].map(x => (isNumber(x) && x) || (isString(x) && parseFloat(x)))

        // wait fot city data load and parse to fullfill
        let trie = await this._trie;
        let tr: TrieSearchResult[] = trie.get(q);

        let minScore: number, maxScore: number;

        // Generate output as required
        let output = tr
            .map(({value}): CitySuggestion => {

                let {name, lat, long, country, region} = value;

                // Generate non-normalized score for every city
                let score = this.calcCityScoreFromCord(value, {latitude, longitude});

                // Do min/max Score captures here for normailzation between 0,1
                if (minScore == null || score < minScore) minScore = score;
                if (maxScore == null || score > maxScore) maxScore = score;

                // Sample Output in repo had lat/long as string, so string here
                return {
                    name: `${name}, ${region}, ${country}`,
                    latitude: lat.toString(),
                    longitude: long.toString(),
                    score,
                }

                // Now all scores have been calculted we can normailze them to fit between 0,1 using min/max
            }).map(suggestion => ({
                ...suggestion,
                score: ((suggestion.score - minScore) / (maxScore - minScore)).toFixed(4)
            })).sort((s1, s2) => s2.score - s1.score)

        // console.log(output);
        return output;
    }
}