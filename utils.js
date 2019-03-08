var self = module.exports = {
    processFile: function(callback) {
        const fs                  = require('fs');
        const csv                 = require('csv-parser');
        const fipscode            = require('./fipscode');
        const countryCode         = require('./country');
        const inputFilePath       = './data/cities_canada-usa.tsv';
        const authorizedCountries = ['US', 'CA'];
        const populationSize      = 5000;
        let results               = [];

        fs.createReadStream(inputFilePath)
        .pipe(csv({separator: "\t"}))
        .on('data', (data) => results.push(data))
        .on('end', () => {
            let filteredData = results
                .filter(d => d.population > populationSize)
                .filter(d => authorizedCountries.indexOf(d.country) !== -1)
                .map(d => {
                    const { name, lat, long, country, admin1 } = d;
                    return {
                        short_name: name,
                        name: `${name}, ${fipscode[country][admin1]}, ${countryCode[country]}`,
                        latitude: lat,
                        longitude: long,
                    }
                });
            
            callback(filteredData);
        });
    },
    searchCity: function(cities, q, latitude = null, longitude = null) {
        let results = self.searchCityByName(cities, q);

        if (Object.keys(results).length === 0) {
            return {};
        }

        // calc distance
        let hasDistance = false;
        if (latitude !== null && longitude !== null) {
            hasDistance = true;
            Object.keys(results).map(function(key, index) {
                results[key].distance = self.calcDistance(results[key], latitude, longitude);
                return results;
            });
        }

        // calc score
        results = self.calcScore(results, q, hasDistance);

        return results.sort( (a, b) => b.score - a.score)
    },
    searchCityByName: function(cities, name) {
        const matchSorter = require('match-sorter');
        return matchSorter(cities, name, {keys: ['name'], threshold: matchSorter.rankings.WORD_STARTS_WITH}).slice(0, 10);
    },
    calcDistance: function(city, latitude, longitude) {
        const turf = require('@turf/turf');
        var from = turf.point([longitude, latitude]);
        var to = turf.point([city.longitude, city.latitude]);
        var options = {units: 'kilometers'};

        return distance = turf.distance(from, to, options);
    },
    calcScore: function(cities, q, hasDistance) {
        if (hasDistance) {
            var distance    = Object.keys(cities).map((key) => cities[key].distance);
            var maxDistance = Math.max.apply(null, distance);

            Object.keys(cities).map( (key) => {
                cities[key].score = (1 - (cities[key].distance / (maxDistance*10))).toFixed(2);
                delete cities[key].short_name;
                delete cities[key].distance;
                return cities;
            });
        } else {
            Object.keys(cities).map( (key) => {
                cities[key].score = (q.length / cities[key].short_name.length).toFixed(2);
                delete cities[key].short_name;
                return cities;
            });
        }

        return cities;
    }
}
