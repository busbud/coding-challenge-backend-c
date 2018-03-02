var utils = require("./utils");

function Store(){
    this.store = {};
    this.cache = {};
}


Store.prototype.init = function (source) {
    var letterFrequencies = utils.findFirstLetterFrequencies(source);
    utils.mergeCitiesByLetterFrequency(source, letterFrequencies, (mergedCities) => {
        this.store = Object.assign({}, mergedCities);
    })
    return this;
}


Store.prototype.query = function(query, cb) {
    var firstLetterOfTerm = query.term[0];
    
    if(this.cache[query.term]) {
        return this.cache[query.term];
    }
    
    if(!firstLetterOfTerm) {
        return [];
    }
    else {
        var cachedCitiesStartingWithLetter = this.store[firstLetterOfTerm].data;
        var citiesStartingWithQuery = cachedCitiesStartingWithLetter.filter(function(city){
            if(city.ascii.match(query.term)) {
                return city;
            }
        });
        
        var suggestions = citiesStartingWithQuery.length > 0 ? utils.computeSuggestions(citiesStartingWithQuery, query) : [];
        this.cache[query.term] = suggestions;
        return this.cache[query.term];
    }
}

module.exports = new Store();