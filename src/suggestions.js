const citiesService = require('./cities');
const geoLib = require('geolib');

module.exports.getSuggestions = async function (req, res) {
    let query = {
        searchString: req.query && req.query.q ? req.query.q.toLowerCase() : null,
        latitude: req.query && req.query.latitude ? req.query.latitude : null,
        longitude: req.query && req.query.longitude ? req.query.longitude : null
    };
    let cities = await citiesService.getCities();
    let hasLocation = false;
    console.log("query is : ", query);

    let results = searchBySearchString(cities, query.searchString);
    if (results && results.length) {
        results = distanceFromGivenCoords(results, query, hasLocation);
    }

    if (query.latitude && query.longitude) { hasLocation = true; }
    results = getCityScores(results, hasLocation);
    results = sortByScores(results);

    console.log(results);



    // res.status(200).send('Hello World!!!');
    let parsedResponse = JSON.stringify(results,null,'\t');
    res.status(200).send(parsedResponse);
}

function searchBySearchString(cities, searchString) {

    let results = cities.filter((city) => {
        if (city.name.startsWith(searchString)) {
            // city = {
            //     name: city.name
            // }
            return city;
        }
    });
    return results;
}

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


/*
    since we don't have any scores, so lets give the scores to the cities based on following weight allocation assumptions
    - 0.7 for population (assuming max population cities should come on top)
    - 0.3 for location (distance from given latitude and longitude, assuming we want nearby places to come on top)
*/
function getCityScores(filteredCities, hasLocation) {
    const maxPopulation = Math.max.apply(Math, filteredCities.map((city) => city.population));
    const nearestDistance = hasLocation ? Math.max.apply(Math, filteredCities.map((city) => city.geoDistance)) : 0;
    filteredCities = filteredCities.map((city) => {
        city.score = (city.population / maxPopulation) * 0.7;
        if (hasLocation) {
            city.score = city.score + (1-(city.geoDistance / nearestDistance) * 0.3);
            delete city.geoDistance;
        }
        return city;
    });

    return filteredCities;
}

function sortByScores(filteredCities) {
    filteredCities.sort((city1, city2) => { return city2.score - city1.score });
    return filteredCities;
}