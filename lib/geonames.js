var fs = require('fs');
var parse = require('csv-parse');

var GeoNames = function (fileName, queryLengthMin, queryResultMax, onReady) {
    this.cities = [];
    this.queryLengthMin = queryLengthMin;
    this.queryResultMax = queryResultMax;
    this.onReady = onReady;
    this.loadCities(fileName);
};

GeoNames.prototype = {
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

    addCity: function (city) {
        this.cities.push(city);
    },

    /*
     * Filter the cities to match the query string
     * @param {type} qs We asume that the qs is already lower case to compare with ascii name of the city 
     * @returns {unresolved}
     */
    filterCities: function (qs) {
        return this.cities.filter(function (city) {
            return city.ascii.indexOf(qs) > -1;
        });
    },

    /**
     * 
     * @param {type} query
     * @param {type} location We asume that the location is valid point or undefined
     * @returns {nm$_geonames.GeoNames.prototype.returnNoCities.geonamesAnonym$2}
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
     * Score and sort cities, if exact match P1, if starts with 2P (ordered by alpahbet), the sort alphabet
     * @param {type} cities
     * @param {type} qs
     * @returns {Object.prototype.scoreCitiesOnName.os|nm$_geonames.GeoNames.prototype.scoreCitiesOnName.os}
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

    scoreCitiesOnLoc: function (cities, qs, loc) {
        var dmax = Math.pow(360, 2) + Math.pow(180, 2);

        var sc = cities.map(function (city) {
            city.score = (dmax - Math.pow(city.latitude - loc.latitude, 2) - Math.pow(city.longitude - loc.longitude, 2)) / dmax;
            return city;
        });

        return sc;
    },

    sortCitiesByScore: function (cities) {
        return cities.sort(function (a, b) {
            return b.score - a.score;
        });
    },

    returnCities: function (cities) {
        return {
            suggestions: this.sortCitiesByScore(cities).slice(0, this.queryResultMax)
        };
    },

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
