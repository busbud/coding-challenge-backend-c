//find the frequency of occurrence
//of the first letters of some data parameter
exports.findFirstLetterFrequencies = function(source, key) {
    var letterFrequencies = {};

    source.map(function(record) {
        var firstLetter = record[key][0];
        //if the letter has been seen before, increment the value by one
        //else set the value to 1
        letterFrequencies[firstLetter] = letterFrequencies[firstLetter] ? (letterFrequencies[firstLetter] += 1) : (letterFrequencies[firstLetter] = 1);
    });

    //sort in descending count order
    var sortedLetterFrequencies = Object.keys(letterFrequencies).map(function(letter) {
            var letterFrequency = {};
            letterFrequency.letter = letter;
            letterFrequency.count = letterFrequencies[letter];
            return letterFrequency;
        })
        .sort(function(a, b) {
            if (a.count < b.count) return 1;
            if (a.count > b.count) return -1;
            return 0;
        });

    return sortedLetterFrequencies;
}

exports.mergeCitiesByLetterFrequency = function(cities, frequencies, cb) {
    var mergedCities = {};
    //get the city data for the cities
    //starting with the top 10 most frequent letters
    //this data will be used to initialize the store
    frequencies.slice(0, 10).map(function(frequency) {
        cities.filter(function(city) {
            if (city.name[0] == frequency.letter) {
                mergedCities[frequency.letter] = !mergedCities[frequency.letter] ? (mergedCities[frequency.letter] = { data: [] }) : mergedCities[frequency.letter];
                mergedCities[frequency.letter].data.push(city);
            }
        })
    });

    cb(mergedCities);
}


exports.computeSuggestions = function(source, query) {

    var sortedByPopulation = source.sort(function(a, b) {
        if (parseInt(a.population) > parseInt(b.population)) return -1;
        if (parseInt(a.population) < parseInt(b.population)) return 1;
        return 0;
    });

    var highestPopulation = sortedByPopulation[0];
    var long = query.longitude;
    var lat = query.latitude;

    var highestPopulationSuggestion = {
        name: highestPopulation.ascii + ", " + highestPopulation.country,
        longitude: highestPopulation.long,
        latitude: highestPopulation.lat,
        population: highestPopulation.population
    }

    var otherSuggestions = sortedByPopulation.slice(1).map(function(city) {
        return {
            name: city.ascii + ", " + highestPopulation.country,
            longitude: city.long,
            latitude: city.lat,
            population: city.population
        }
    });
    
    /*
        uses default lan and long data (Montreal).
        score broken into three parts
            - geoScore -> calculated by taking the ratio of the 
                          city's own geo information to the default geo information
                          and computing the average
                          
            - termScore-> calculated using the length of the search query versus the city's
                          name length. if the search query's length is greater than the 
                          name of the city, we dock the score to 0.1 to indicate a loss of relevance
                          in the result
            
            - populationScore -> ratio of the city's population to the city in the result set with the 
                                 highest population. Score is biased to city with higher population in
                                 case cities share the same name.
    */
    function calculateScore(ln, lat, term) {
        var latScore    = 0;
        var lnScore     = 0;
        var termScore   = 0;
        var geoScore    = 0;
        
        var populationScore = 0;

        var absCityLn = Math.abs(parseFloat(this.longitude));
        var absLn = Math.abs(parseFloat(ln))

        var absCityLat = Math.abs(parseFloat(this.latitude));
        var absLat = Math.abs(parseFloat(lat))

        if (absLn >= absCityLn) {
            lnScore = absCityLn / absLn;
        }
        else {
            lnScore = absLn / absCityLn;
        }

        if (absLat >= absCityLat) {
            latScore = absCityLat / absLat;
        }
        else {
            latScore = absLat / absCityLat;
        }

        geoScore    = (lnScore + latScore) / 2;
        termScore   = term.length > this.name.length ? 0.1 : term.length / this.name.length;
        
        populationScore = parseInt(this.population) / parseInt(highestPopulation.population);
        
        if (geoScore > 0.5 && ln && lat) {
            this.score = parseFloat(geoScore.toFixed(1));
        }
        else {
            var score = geoScore * termScore * populationScore;
            this.score = score < 0.1 ? 0.1 : parseFloat(score.toFixed(1));
        }
        delete this.population;
        return this;
    }
    
    var suggestions = otherSuggestions.concat(highestPopulationSuggestion).map(function(suggestion) {
        return calculateScore.call(suggestion, long, lat, query.term);
    });

    return suggestions;
}
