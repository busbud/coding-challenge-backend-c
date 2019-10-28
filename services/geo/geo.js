const fs = require('fs');
const readline = require('readline');
const es = require('event-stream');
const FuzzySearch = require('fuzzy-search');

const CITY_DB = [];

/*
    Searches the CITY_DB for objects whose 'name' paramter,
    matches the query

    @param query - the user supplied query
*/
function find(query, cb) {
    const searcher = new FuzzySearch(CITY_DB, ['name'], {});
    cb(searcher.search(query || ''));
}

/*
    given the latitude and longitude of two points, return the
    distance in KM between the two points

    @param lat1 - the latitude of the first point
    @param lng1 - the longitude of the first point
    @param lat2 - the latitude of the second point
    @param lng2 - the longitude of the second point
*/
function calculateDistance(lat1, lng1, lat2, lng2) {
    let R = 6371; // Radius of the earth in km
    let dLat = (lat1-lat2) * (Math.PI/180);
    let dLon = (lng1-lng2) * (Math.PI/180);
    let a = Math.sin(dLat/2) *
        Math.sin(dLat/2) +
        Math.cos(lat1 * (Math.PI/180)) *
        Math.cos(lat2 * (Math.PI/180)) *
        Math.sin(dLon/2) *
        Math.sin(dLon/2);
    let c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    return R * c;
}

/*
    Pull all the city objects from the file and load them into an array that
    will be used as an in memory database.
*/
function _loadCities(cb) {
    const stream = fs.createReadStream(__dirname+'/cities_canada-usa.tsv');
    const readInterface = readline.createInterface({
        input: stream
    });

    let attributes = [];
    let isFirstLine = true;

    // iterate through the file line by line
    readInterface.on('line', function(line) {
        let values = line.split('\t');
        // The first line of the file has the attribute names of the records
        if(isFirstLine) {
            attributes = values;
            isFirstLine = false;
        } else {
            // For each line item, create an object using the attribute names
            // and their associated values
            let cityObject = attributes.reduce(function(object, attribute, index) {
                object[attribute] = values[index];
                return object;
            }, {});

            // store the city object format that our application understands
            // and expects. Remove unnecessary attributes
            cityObject = {
                'name': [cityObject['ascii'],
                    cityObject['admin1'],
                    cityObject['country']
                ].join(','),
                'latitude': cityObject['lat'],
                'longitude': cityObject['long']
            }
            CITY_DB.push(cityObject);
        }
    });
}
_loadCities();

module.exports = {
    find: find,
    calculateDistance: calculateDistance
};
