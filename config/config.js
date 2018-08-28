var config = {};

config.port = process.env.PORT || 2346;
config.dataStore = {
    source: 'data/cities_canada-usa.tsv',
    columnIndexMapping: {
        id: 0,
        name: 1,
        cityASCII: 2,
        alternativeName: 3,
        latitude: 4,
        longitude: 5,
        type: 6,
        country: 8,
        state: 10,
        population: 14,
    },
    canadianProvinceNameMap: {
        '01': 'AB',
        '02': 'BC',
        '03': 'MB',
        '04': 'NB',
        '05': 'NL',
        '07': 'NS',
        '08': 'ON',
        '09': 'PE',
        '10': 'QC',
        '11': 'SK',
        '12': 'YK',
        '13': 'NT',
        '14': 'NU'
    }
};

config.suggestionEngine = {

    // maximum number of suggestions to be returned to the client
    maxSuggestions: 5,

    // distance after which score is based only on city name
    // if client send lat/long it means he in interested only in specific area
    maxDistanceRadiusInMeter: 1000000 //1000 km 
}
config.loadTest = {
    // loader.io validation token
    loaderIOValidationToken: 'loaderio-f5bb43ba1f1d3ab815f83e71551194fe'
}
module.exports = config;