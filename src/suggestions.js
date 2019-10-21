const citiesService = require('./cities');
const geoLib = require('geolib');

/*
    Since we don't have any scores, so lets give the scores to the cities based on following weight allocation assumptions
    # if no location coordinates are provided: 
        - 0.8 for search string, i.e., more closer the name w.r.t given search string, more relevant that city is,
        - 0.2 for population (as given this is auto completion suggestion api for larger cities, so assuming larger the
            population more the significance of the city)
    # if location coordinates are provided:
        - 0.5 for combination of search string and population
            (breakdown: 0.3 for searchString and 0.2 for population)
        - 0.5 for location (distance from given latitude and longitude, assuming we want nearby places to come on top)
    
*/
const distributedWeightIfNoLocation = {
    searchString: 0.8,
    population: 0.2
}

const distributedWeightBasedOnLocation = {
    searchString: 0.5,
    location: 0.5
}

// gets suggestions
module.exports.getSuggestions = async function (req, res) {
    let query = req.query || {};
    let cities = await citiesService.getCities();
    let hasLocation, validGivenCoords = false;

    if (query.latitude && query.longitude) { 
        hasLocation = true;    
        validGivenCoords = geoLib.isValidCoordinate({latitude: query.latitude, longitude: query.longitude})    
    }

    let results = [];
    if (query.q) {
        results = searchCitiesAndGetSuggestions(cities, query, hasLocation, validGivenCoords);
    }

    if (results && results.length) {
        res.status(200).send({ suggestions: results });
    } else {
        res.status(404).send({ suggestions: [] });
    }
}

// search for given query params and prepare a suggested list of suggestions 
function searchCitiesAndGetSuggestions(cities, query, hasLocation, validGivenCoords) {
    let searchString = query.q.toLowerCase()
    let results = searchBySearchString(cities, searchString) || [];

    if(results && results.length) {
        results = distanceFromGivenCoords(results, query, hasLocation, validGivenCoords);
        results = getCityScores(results, hasLocation, searchString);
        results = prepareAndSortSuggestedCities(results);
    }

    return results;
}


// getting search results based on given search string as query param
function searchBySearchString(cities, searchString) {

    let results = cities.filter((city) => {
        const cityName = city.name.toLowerCase();
        if (cityName.startsWith(searchString)) {
            return city;
        }

        if (city.alt_name && city.alt_name.length) {
            const filteredAltNames = city.alt_name.filter((altName) => {
                if (altName.toLowerCase().startsWith(searchString)) {
                    return altName;
                }
            });

            if (filteredAltNames && filteredAltNames.length) {
                city.name = filteredAltNames[0];
                return city;
            }
        }
    });

    // console.log(results)
    return results;
}

// calculating the distance from given coordinates(helper function)
function distanceFromGivenCoords(results, query, hasLocation, validGivenCoords) {
    if (hasLocation && validGivenCoords) {
        results.forEach(result => {
            result.geoDistance = geoLib.getDistance(
                { latitude: result.lat, longitude: result.long },
                { latitude: query.latitude, longitude: query.longitude }
            );
        });
    }
    return results;
}


// calculating city scores
function getCityScores(filteredCities, hasLocation, searchString, validGivenCoords) {
    const maxPopulation = Math.max.apply(Math, filteredCities.map((city) => city.population));
    const farthestDistance = hasLocation ? Math.max.apply(Math, filteredCities.map((city) => city.geoDistance)) : 0;
    filteredCities = filteredCities.map((city) => {
        city.score = (searchString.length/city.name.length) * distributedWeightIfNoLocation.searchString;
        city.score = city.score + ((city.population / maxPopulation) * distributedWeightIfNoLocation.population);
        if (hasLocation && validGivenCoords) {
            city.score = city.score * distributedWeightBasedOnLocation.searchString;
            city.score = city.score + (1 - (city.geoDistance / farthestDistance)) * distributedWeightBasedOnLocation.location;
        }
        return city;
    });

    return filteredCities;
}

// preparing and sorting suggestions array by score
function prepareAndSortSuggestedCities(suggestions) {
    let suggestedCities = suggestions.map((suggestedCity) => {
        return ({
            name: getUniqueCityName(suggestedCity),
            latitude: suggestedCity.lat,
            longitude: suggestedCity.long,
            score: suggestedCity.score
        });
    });
    suggestedCities.sort((city1, city2) => { return city2.score - city1.score });
    return suggestedCities;
}

function getUniqueCityName(city) {
    if(city.country === 'US') {
        return [city.name, city.admin1, city.country].join(', ');
    } else {
        return [city.name, city.country].join(', ');
    }
}
