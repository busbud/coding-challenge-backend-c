var cityIndex = require("./cityIndex");
var cityLoader = require("./cityLoader");
var es = require("event-stream");
var scoreCalculator = require("./scoreCalculator");
var sort = require("sort-stream");

/**
 * Initializes the city search engine
 * @param {string*} dataFilePath 
 * @param {Function} callback - a function called when engine is ready to use. Engine is given to the callback
 */
function init(dataFilePath, callback) {
    if (typeof dataFilePath === 'function') {
        callback = dataFilePath;
        dataFilePath = null;
    }

    var index = cityIndex.createIndex();
    var res = cityLoader.loadAndStoreTo(index, dataFilePath);

    res.on("end", function() {
        callback(new Engine(index));
    });
}

/**
 * City search engine
 * @class
 */
function Engine(index) {
    this.cityIndex = index;
}

/**
 * Returns a readable stream
 * @param {Object} query - A query object like {q:string, latitude:int, longitude:int}
 */
Engine.prototype.searchStream = function(query) {
    return scoreCalculator.computeScores(this.cityIndex.findCities(query.q), query.q, query)
        .pipe(sort(function(city1, city2) {
            if (city1.score == city2.score) {
                if (city1.name.length == city2.name.length) {
                    return city1.distance < city2.distance ? -1 : 1;
                }
                return city1.name.length < city2.name.length ? -1 : 1;
            }
            return city1.score < city2.score ? 1 : -1;
        }))
        .pipe(es.mapSync(function(city) {
            return {
                name: [ city.name, city.state, city.country ].join(", "),
                latitude: city.latitude,
                longitude: city.longitude,
                score: city.score,
                distance: city.distance
            }
        }));
};

module.exports.init = init;