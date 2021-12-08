var fs = require('fs');
var d3 = require('d3-dsv');
var citiesServices = require('./city.services');
var score = require('./score');
const utils = require('./utils');

const getCities = async (name) => {
    const cities = await citiesServices.getAllCities();

    // first, filter on names to reduce 
    // in real, DB/Elastic does it
    const lowerName = name.toLowerCase();
    let result = cities.filter(c => 
        // remove other than CA and USA
        (c.country === 'CA'||
        c.country === 'US' )&&
        // match on name
        c.name.toLowerCase().startsWith(lowerName) ||
        // match on ascii name (special accent)
        c.ascii.toLowerCase().startsWith(lowerName) ||
        // match on alternate names
        // first, the first name
        c.alternatenames.toLowerCase().startsWith(lowerName)||
        // check all others in string
        c.alternatenames.toLowerCase().indexOf(`,${lowerName}`)>0);
    return result;
}

const getSuggestions = async (name, latitude, longitude) => {
    const cities = await getCities(name);

    // calculate score for all we agreed are a match
    const r = cities.map(c => {
        return {
            score: score.scoreCoordAndName(c, name, latitude, longitude),
            name: `${c.name}, ${c.code}, ${utils.getCountry(c.country)}`,
            country: c.country,
            latitude: c.lat,
            longitude: c.long
        }
    });
    
    return r.sort((a,b) => b.score - a.score);
}

module.exports = { getSuggestions }