exports.findFirstLetterFrequencies = function(cities) {
    var letterFrequencies = {};

    cities.map(function(city) {
        var firstLetter = city.name[0];
        letterFrequencies[firstLetter] = letterFrequencies[firstLetter] ? (letterFrequencies[firstLetter] += 1) : (letterFrequencies[firstLetter] = 1);
    });

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
        });;

    return sortedLetterFrequencies;
}

exports.mergeCitiesByLetterFrequency = function(cities, frequencies, cb) {
    //choose top 10 
    var mergedCities = {};

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
    function calculateScore(ln, lat, term) {
        var latScore = 0;
        var lnScore = 0;
        var termScore = 0;
        var geoScore = 0;
        
       
        var absCityLn = Math.abs(parseFloat(this.longitude));
        var absLn = Math.abs(parseFloat(ln))
         
        var absCityLat = Math.abs(parseFloat(this.latitude));
        var absLat = Math.abs(parseFloat(lat))
        
        if(absLn >= absCityLn) {
            lnScore = absCityLn / absLn;
        }
        else {
            lnScore = absLn / absCityLn;
        }
        
        if(absLat >= absCityLat) {
            latScore = absCityLat / absLat;
        }
        else {
            latScore = absLat / absCityLat;
        }
        
        geoScore = (lnScore + latScore) / 2;
        termScore = term.length / this.name.length;
        
        if(geoScore > 0.5 && ln && lat) {
            this.score = parseFloat(geoScore.toFixed(1));
        }
        else {
            var score = geoScore  * termScore * parseInt(this.population) / parseInt(highestPopulation.population);
            this.score =  score < 0.1 ? 0.1 : parseFloat(score.toFixed(1));
        }
        delete this.population;
        return this;
    }
    
    var sortedByPopulation = source.sort(function(a, b) {
        if (parseInt(a.population) > parseInt(b.population)) return -1;
        if (parseInt(a.population) < parseInt(b.population)) return 1;
        return 0;
    });

    var highestPopulation = sortedByPopulation[0];
    var long  = query.longitude;
    var lat   = query.latitude;
    
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
    
    var suggestions = otherSuggestions.concat(highestPopulationSuggestion).map(function(suggestion){
        return calculateScore.call(suggestion, long, lat, query.term);
    });
    
    return suggestions;
}
