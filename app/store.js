var utils = require("./utils");

function Store() {
    this.store = {};
    this.cache = {};
}

//seed the data store with the cities
//starting the the top 10 most frequent letters
//in the data source
Store.prototype.init = function(source) {
    this.source = source;
    var letterFrequencies = utils.findFirstLetterFrequencies(source, "ascii");
    utils.mergeCitiesByLetterFrequency(source, letterFrequencies, (mergedCities) => {
        this.store = Object.assign({}, mergedCities);
    })
    return this;
}

//query the data store
//and cache the returned suggestions
//using the query term as key
Store.prototype.query = function(query) {
    var firstLetterOfTerm = query.term[0].toUpperCase();
    if (!firstLetterOfTerm) {
        return [];
    }
    //if the query term has been cached
    //return the cached suggestions
    if (this.cache[query.term]) {
        return this.cache[query.term];
    }
    
    //when a query is received
    //find the data set containing all cities starting
    //with the first letter of the query
    //and filter the set by matching the full query term
    //to the city's name
    var storedCitiesStartingWithLetter = this.store[firstLetterOfTerm] ? this.store[firstLetterOfTerm].data : (this.store[firstLetterOfTerm] = this.fetch(firstLetterOfTerm).data);
    var citiesStartingWithQuery = storedCitiesStartingWithLetter.filter(function(city) {
        if (city.ascii.match(query.term)) {
            return city;
        }
    });

    var suggestions = citiesStartingWithQuery.length > 0 ? utils.computeSuggestions(citiesStartingWithQuery, query) : [];
    this.cache[query.term] = suggestions.sort(function(a, b){
        if(a.score > b.score) return -1;
        if(a.score < b.score) return 1;
        return 0;
    });
    return this.cache[query.term];
}

//for getting city data
//on cities starting with a letter
//that doesn't appear frequently
//as the first letter of the city names
Store.prototype.fetch = function(letter) {
    var citiesStartingWithLetter = this.source.filter(function(city){
        if(city.ascii[0] == letter) {
            return city;
        }
    });
    
    var storeEntry  = {};
    storeEntry.data = citiesStartingWithLetter;
    
    this.store[letter] = storeEntry;
    return storeEntry;
}

module.exports = new Store();
