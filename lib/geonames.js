var fs = require('fs');
var parse = require('csv-parse');

/**
 * GeoNames module constructor
 * @param {string} fileName The file name containing data of cities
 * @param {number} queryLengthMin The minimal query string length when using a search
 * @param {number} queryResultMax Tha maximum nmber of elements returned when a match is found
 * @param {type} onReady The callback when the loading has finished
 */
var GeoNames = function (fileName, queryLengthMin, queryResultMax, onReady) {
    this.cities = [];
    this.queryLengthMin = queryLengthMin;
    this.queryResultMax = queryResultMax;
    this.onReady = onReady;
    this.loadCities(fileName);
};

GeoNames.prototype = {
    /*
     * Load the data file of cities
     * @param {type} fileName The file name containing data of cities
     */
    loadCities: function (fileName) {
        var self = this;

        // Create the parser
        parser = parse({auto_parse: true, delimiter: '\t', columns: true, trim: true, skip_empty_lines: true, quote: false});

        // Catch any error
        parser.on('error', function (err) {
            console.log('GeoNames error ' + err.message);
        });

        // When we are done, test that the parsed output matched what expected
        parser.on('finish', function () {
            console.log('GeoNames has loaded ' + self.cities.length + ' cities.');

            if (self.onReady)
                self.onReady(self);
        });

        parser.on('readable', function () {
            var record;

            while ((record = parser.read())) {
                if ((record.country !== 'CA' && record.country !== 'US') || (record.population < 5000))
                    continue;
                self.addCity({
                    ascii: record.ascii.toLowerCase(),
                    name: record.name + ', ' + record.country,
                    latitude: record.lat,
                    longitude: record.long,
                    score: 0
                });
            }
        });

        console.log('GeoNames loading cities...');
        fs.createReadStream(fileName).pipe(parser);
    },
    
    /*
     * Add new city to the array of cities to search for
     * @param {type} city
     */
    addCity: function (city) {
        this.cities.push(city);
    },

    /*
     * Filter the cities to match the query string
     * @param {string} qs The query string that should be already in lower case to compare with ascii name of the city 
     * @returns {Object[]} The cities containing in their names the query string
     */
    filterCities: function (qs) {
        return this.cities.filter(function (city) {
            return city.ascii.indexOf(qs) > -1;
        });
    },

    /**
     * Search for a city, location is optional (latitude and longitude) to help improve relative scores
     * @param {string} query The partial (or complete) search term
     * @param {Object} location The caller's location (optional)
     * @returns {Object} the seggestions result of the cities
     */
    searchCity: function (query, location) {
        if ('undefined' === typeof query)
            return this.returnCities([]);

        var qs = query.trim().toLowerCase();

        if (qs.length < this.queryLengthMin)
            return this.returnCities([]);

        var qr = this.filterCities(qs);
        if (!this.isLocation(location)) {
            console.log('GeoNames search city with "' + qs + '"...');
            return this.returnCities(this.scoreCitiesOnName(qr, qs));
        }

        console.log('GeoNames search city with "' + qs + '" near "' + JSON.stringify(location) + '"...');
        return this.returnCities(this.scoreCitiesOnLoc(qr, qs, location));
    },

    /**
     * Score and sort cities based only on name search.
     * If there is an exact match, it is on the top of the list.
     * Next we put the cities starting with the query string ordered by alpahbet.
     * Next we put the cities containing the query string ordered by alphabet.
     * @param {Object[]} cities The cities containing in their names the query string
     * @param {string} The partial (or complete) search term
     * @returns {Object[]} The scored and ordered cities.
     */
    scoreCitiesOnName: function (cities, qs) {
        var os = cities.sort(function (a, b) {
            var na = a.ascii;
            var nb = b.ascii;

            if (((na === qs) || (na.indexOf(qs) === 0)) && (na !== nb)) {
                return -1;
            }
            if (((nb === qs) || (nb.indexOf(qs) === 0)) && (na !== nb)) {
                return 1;
            }
            if (na > nb) {
                return 1;
            }
            if (na < nb) {
                return -1;
            }
            return 0;
        });

        var ol = os.length;
        os.map(function (city, index) {
            city.score = (ol - index) / ol;
            return city;
        });

        return os;
    },
    
    /**
     * Score and sort cities based on name search and valid location.
     * For simpler implementation and fast procesing, we consider that the Earth is "flat" on a coordinate system to calculate the distance between 2 points.
     * We use latitude and longitude as y and x on the coordinate system and pythagorean theorem calculator.
     * For real result we can implement https://www.movable-type.co.uk/scripts/latlong.html
     * @param {type} cities The cities containing in their names the query string
     * @param {type} qs The partial (or complete) search term
     * @param {type} loc The caller's location parameters latitude and longitude to help improve relative scores
     * @returns {Object[]} The scored and ordered cities.
     */
    scoreCitiesOnLoc: function (cities, qs, loc) {
        var dmax = Math.pow(360, 2) + Math.pow(180, 2);

        var sc = cities.map(function (city) {
            city.score = (dmax - Math.pow(city.latitude - loc.latitude, 2) - Math.pow(city.longitude - loc.longitude, 2)) / dmax;
            return city;
        });

        return sc;
    },
    
    /**
     * Sort the array of sities based on their relative score
     * @param {Object[]} cities to sort
     * @returns {Object[]} Cities ordered by descending score
     */
    sortCitiesByScore: function (cities) {
        return cities.sort(function (a, b) {
            return b.score - a.score;
        });
    },
    
    /**
     * The JSON object containing scored cities that respects the API design
     * Exemple of one city: {"suggestions": [{"name": "London, ON, Canada", "latitude": "42.98339", "longitude": "-81.23304", "score": 0.9}]}
     * @param {Object[]} cities to return
     * @returns {Object} The suggestions JSON object
     */
    returnCities: function (cities) {
        return {
            suggestions: this.sortCitiesByScore(cities).slice(0, this.queryResultMax)
        };
    },
    
    /**
     * Check of location is valid
     * @param {Object} location to check
     * @returns {Boolean} if the location is valid
     */
    isLocation: function (location) {
        if (undefined !== location && undefined !== location.latitude && undefined !== location.longitude) {
            var la = Number(location.latitude);
            var lo = Number(location.longitude);

            return typeof (la) === 'number' && typeof (lo) === 'number' && !isNaN(la) && !isNaN(lo) && la >= -90 && la <= 90 && lo >= -180 && lo <= 180;
        }

        return false;
    }
};

module.exports = GeoNames;
