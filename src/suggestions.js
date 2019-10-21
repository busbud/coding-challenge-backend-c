const citiesService = require('./cities');
const geoLib = require('geolib');

/*
    since we don't have any scores, so lets give the scores to the cities based on following weight allocation assumptions
    - 0.3 for population (assuming max population cities should come on top)
    - 0.7 for location (distance from given latitude and longitude, assuming we want nearby places to come on top)
*/
const distributedWeight = {
    population: 0.3,
    location: 0.7
}

// gets suggestions
module.exports.getSuggestions = async function (req, res) {
    let query = req.query || {};
    let cities = await citiesService.getCities();
    let hasLocation = false;

    if (query.latitude && query.longitude) { 
        hasLocation = true; 
    }

    let results = searchCitiesAndGetSuggestions(cities, query, hasLocation);

    if (results && results.length) {
        
        res.status(200).send({ suggestions: results });
    } else {
        res.status(404).send({ suggestions: [] });
    }
}

// search for given query params and prepare a suggested list of suggestions 
function searchCitiesAndGetSuggestions(cities, query, hasLocation) {
    let results = searchBySearchString(cities, query.q.toLowerCase()) || [];

    if(results && results.length) {
        results = distanceFromGivenCoords(results, query, hasLocation);
        results = getCityScores(results, hasLocation);
        results = prepareAndSortSuggestedCities(results);
    }

    return results;
}


// getting search results based on given search string as query param
function searchBySearchString(cities, searchString) {
    console.log(searchString);
    let results = cities.filter((city) => {
        const cityName = city.name.toLowerCase();
        const alt_name = city.alt_name.toLowerCase();
        if (cityName.startsWith(searchString)) {
            return city;
        }
        if (alt_name.startsWith(searchString)) {
            city.name = city.alt_name;
            return city; 
        }
    });

    // console.log(results)
    return results;
}

// calculating the distance from given coordinates(helper function)
function distanceFromGivenCoords(results, query, hasLocation) {
    if (hasLocation) {
        results.forEach(result => {
            result.geoDistance = geoLib.getDistance(
                { latitude: query.latitude, longitude: query.longitude },
                { latitude: result.lat, longitude: result.long }
            );
        });
    }
    return results;
}


// calculating city scores
function getCityScores(filteredCities, hasLocation) {
    const maxPopulation = Math.max.apply(Math, filteredCities.map((city) => city.population));
    const farthestDistance = hasLocation ? Math.max.apply(Math, filteredCities.map((city) => city.geoDistance)) : 0;
    filteredCities = filteredCities.map((city) => {
        city.score = (city.population / maxPopulation) * distributedWeight.population;
        if (hasLocation) {
            city.score = city.score + (1 - ((city.geoDistance / farthestDistance) * distributedWeight.location));
            delete city.geoDistance;
        }
        return city;
    });

    return filteredCities;
}

// preparing and sorting suggestions array by score
function prepareAndSortSuggestedCities(suggestions) {
    let suggestedCities = suggestions.map((suggestion) => {
        return ({
            name: suggestion.name,
            alt_name: suggestion.alt_name,
            latitude: suggestion.lat,
            longitude: suggestion.long,
            score: suggestion.score
        });
    });
    suggestedCities.sort((city1, city2) => { return city2.score - city1.score });
    return suggestedCities;
}
