'use strict';

const parse = require('csv-parse/lib/sync');
const fs = require('fs');
const haversine = require('haversine');
const natural = require('natural');
const Trie = natural.Trie;

const comparisonFunction = (a,b) => {
    return b.score - a.score;
}

//Class that can be used to grab closest cities for a query
class Cities{
    constructor(){
        //hashmap to map city names to longitude/latitude
        this.hash = {}
        this.trie = new Trie();
        //normally, I would use the async call but both for time
        //reasons and because the file isn't *that* large, a sync
        //call should be fine
        let citiesRaw = fs.readFileSync('data/cities_canada-usa.tsv').toString();
        let cities = parse(citiesRaw, {
            delimiter: '\t',
            columns: true,
            escape: '\\',
            quote: null
        });
        for(let city of cities){
            if(city.population > 5000){
                //Add the variations people are likely to query
                //to the trie
                const objectToKeyTo = { id: city.id,
                    lat: parseFloat(city.lat), long: parseFloat(city.long)};
                if(city.alt_name !== ''){
                    const altNameMatch = city.alt_name + ', ' + city.admin1 + ', ' + city.country;
                    this.trie.addString(city.alt_name + ', ' + city.admin1 + ', ' + city.country);
                    this.hash[altNameMatch] = objectToKeyTo;
                }
                const asciiMatch = city.ascii + ', ' + city.admin1 + ', ' + city.country;
                const cityNameMatch = city.name + ', ' + city.admin1 + ', ' + city.country;
                this.trie.addString(city.ascii + ', ' + city.admin1 + ', ' + city.country);
                this.trie.addString(city.name + ', ' + city.admin1 + ', ' + city.country);
                //Map city names to position
                this.hash[asciiMatch] = objectToKeyTo;
                this.hash[cityNameMatch] = objectToKeyTo;
            }
        }
    }

    queryTrie(queryString){
        //I use a trie in order to increase the effiency of finding
        //the relevant strings
        return this.trie.keysWithPrefix(queryString);
    }

    //This reduces code duplication
    runCallbackOnMatches(queryString, queryCallback){
        const matches = this.queryTrie(queryString);
        const results = [];
        const seenIds = new Set();
        for(let match of matches){
            if(!seenIds.has(match.id)){
                queryCallback(match);
            }
            seenIds.add(match.id);
        }
    }

    determineResultObjectFromMatch(match, queryString){
        const similarityScore = natural.JaroWinklerDistance(match, queryString);
        return {
            name: match,
            longitude: this.hash[match].long,
            latitude: this.hash[match].lat,
            score: similarityScore
        }
    }

    determineResultObjectFromMatchAndLocation(match, queryString, location){
        const similarityScore = natural.JaroWinklerDistance(match, queryString);
        const distanceScore = haversine(location, {
            longitude: this.hash[match].long,
            latitude: this.hash[match].lat
        });
        return {
            name: match,
            longitude: this.hash[match].long,
            latitude: this.hash[match].lat,
            score: (similarityScore + distanceScore) / 2
        }
    }

    queryWithoutLocation(queryString){
        const results = [];
        this.runCallbackOnMatches(queryString, (match) => {
            results.push(this.determineResultObjectFromMatch(match, queryString));
        });
        //sort the array with the score descending
        results.sort(comparisonFunction);
        return results;
    }

    queryWithLocation(queryString, longitude, latitude){
        const results = [];
        const inputLocation = {
            longitude: longitude,
            latitude: latitude
        }
        this.runCallbackOnMatches(queryString, (match) => {
            results.push(this.determineResultObjectFromMatchAndLocation(match, queryString, inputLocation));
        });
        results.sort(comparisonFunction)
        return results;
    }
}

module.exports.Cities = Cities;