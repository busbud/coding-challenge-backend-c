import {getDataAndMakeTrie, ParsedCityData, TrieSearchResult} from './geonames-import';
// For type
import TrieSearch = require('trie-search');
import {IncomingMessage} from "http";

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

export class util {

    private _trie: Promise<TrieSearch>
    private _connections = {};


    constructor() {
        this._trie = getDataAndMakeTrie();
    }

    public calcCityScoreFromCord(cityData: ParsedCityData, cord2: Coordinate) {
        let {population, long, lat} = cityData;
        let {latitude, longitude} = cord2;
        let score = 0;
        // Note: This is not very accurate calculation, as it doesn't take into account the curvature of the earth
        // but for this context, it will suffice
        if (latitude && longitude) {
            score = Math.sqrt(Math.pow((longitude - long), 2) - Math.pow((latitude - lat), 2))
        }
        // Final score add emphasis on distance form long/lat and small boost for population
        return (score + 1) ** 2 + (population / 10000000);
    }

    public async thottleConnection(req: IncomingMessage) {
        // No connection for this IP , register one with 300 timeout
        if (!this._connections[req.connection.remoteAddress]) {
            this._connections[req.connection.remoteAddress] = new Promise((resolve, reject) => {
                setTimeout(resolve, 300);
            });
            // Return a true for first connection
            return Promise.resolve(true);
        }
        else {
            console.log('Connection Throttle Activated for', req.connection.remoteAddress, 'waiting..');
            // Ip already has connection promise, wait for it to resolve
            await this._connections[req.connection.remoteAddress];
            console.log('Connection released');
            return true;
        }
    }

    public getSuggestionsFromRequest = async (requestParams: any): Promise<CitySuggestion[]> => {
        let {q, latitude, longitude} = requestParams;

        if (!q || q.length < 3)
            return [];

        if (latitude && longitude) {
            latitude = (!isNaN(latitude) && latitude) || (latitude.length && parseFloat(latitude));
            longitude = (!isNaN(longitude) && longitude) || (longitude.length && parseFloat(longitude));
        }

        let trie = await this._trie;
        let tr: TrieSearchResult[] = trie.get(q);

        let minScore: number, maxScore: number;

        // Generate output as required
        let output = tr.map(({value}): CitySuggestion => {

            let {name, lat, long, country} = value;

            // Generate non-normalized score for every city
            let score = this.calcCityScoreFromCord(value, {latitude, longitude});

            // Do min/max Score captures here for normailzation between 0,1
            if (minScore == null || score < minScore)
                minScore = score;
            if (maxScore == null || score > maxScore)
                maxScore = score;

            return {
                name: `${name}, ${country}`,
                latitude: lat.toString(),
                longitude: long.toString(),
                score,
            }

            // Now all scores have been calculted we can normailze them to fit between 0,1 using min/max
        }).map(suggestion => ({
            ...suggestion,
            score: ((suggestion.score - minScore) / (maxScore - minScore))
        }))
        // console.log(output);
        return output;


    }
}