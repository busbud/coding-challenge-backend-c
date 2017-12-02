const csvToJson = require("convert-csv-to-json");
const City = require('../models/City');
const CityMatch = require('../models/CityMatch');
const fuzz = require('fuzzball');

/**
 * Load all the cities into a global variable "cities"
 * Use of a global variable is not a problem because we only want to have read access on this variable
 * @param csvFilePath
 * @param delimiter
 */
exports.loadCities = function(csvFilePath, delimiter) {
    global.cities = [];

    let json = csvToJson.fieldDelimiter(delimiter).getJsonFromCsv(csvFilePath);
    for(let i=0; i<json.length;i++){
        global.cities.push(new City(json[i]));
    }
}

exports.findCities = function(partialCityToFind, minimumScore) {
    const minScore = (minimumScore !== undefined && minimumScore <= 1 && minimumScore  >= 0)?minimumScore:50;
    let results = [];
    console.log(minScore);
    for(var i = 0; i< global.cities.length;i++) {
        let score = fuzz.ratio(global.cities[i].name, partialCityToFind);
        if(score >= minScore*100) {
            results.push(new CityMatch(global.cities[i], score, 0));
        }
    }
    return results;
}