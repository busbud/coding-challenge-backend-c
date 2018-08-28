var fs = require('fs');
const util = require("util");
const CityModel = require('../models/city.model');
const Logger = require('../utils/logger.service');
const StringManipulationService = require('../utils/string-manipulation.service');
const config = require('../config/config');
const logger = new Logger();
const utilityService = new StringManipulationService();
class DataStoreService {

    constructor() {
        this.cities = [];
        this.suggestedCityMap = [];
        this.initialized = false;
        this.count = 0;
    }

    createCityIndex(cityDetails) {
        const mappedCity = new CityModel(
            cityDetails[config.dataStore.columnIndexMapping.id],
            cityDetails[config.dataStore.columnIndexMapping.name],
            cityDetails[config.dataStore.columnIndexMapping.cityASCII],
            cityDetails[config.dataStore.columnIndexMapping.alternativeName],
            cityDetails[config.dataStore.columnIndexMapping.latitude],
            cityDetails[config.dataStore.columnIndexMapping.longitude],
            cityDetails[config.dataStore.columnIndexMapping.country],
            cityDetails[config.dataStore.columnIndexMapping.state])
        this.cities[mappedCity.id] = mappedCity;

        // create alternative name index
        const cityAlternativeNames = mappedCity.alternativeNames.split(',');
        cityAlternativeNames.forEach(altName => {
            this.addIndexRecord(altName, mappedCity);
        });

        // create city name index
        if (cityAlternativeNames.indexOf(mappedCity.name) === -1) {
            this.addIndexRecord(mappedCity.name, mappedCity);
        }
        // create city ascii index
        if (cityAlternativeNames.indexOf(mappedCity.cityASCII) === -1) {
            this.addIndexRecord(mappedCity.cityASCII, mappedCity);
        }
    }
    addIndexRecord(name, mappedCity) {
        for (var j = name.length; j > 0; j--) {
            var searchKey = name.slice(0, j).toUpperCase();
            searchKey = utilityService.removeDashAndWhiteSpace(searchKey);
            if (typeof this.suggestedCityMap[searchKey] === "undefined") {
                this.suggestedCityMap[searchKey] = [{
                    id: mappedCity.id,
                    nameScoreLength: utilityService.removeDashAndWhiteSpace(name).length
                }];
                this.count++;
            } else if (!this.suggestedCityMap[searchKey].some(x => x.id === mappedCity.id)) {

                this.suggestedCityMap[searchKey].push({
                    id: mappedCity.id,
                    nameScoreLength: utilityService.removeDashAndWhiteSpace(name).length
                });
                this.count++;
            }
        }
    }

    /**
     * Initialize data store
     *
     * @returns
     * @memberof DatabaseService
     */
    initialize() {
        if (this.initialized) return;

        const readFile = util.promisify(fs.readFile);
        return readFile(config.dataStore.source, 'utf8')
            .then((fileContent) => {
                const cityRecords = fileContent.split('\n');
                logger.log('raw cities: ' + (cityRecords.length - 1));
                for (let i = 1; i < cityRecords.length; i++) { // ignore first line (column names)
                    const currentCity = cityRecords[i];
                    const cityDetails = currentCity.split('\t');

                    // filter by type "city" and population > 5000
                    if (cityDetails[config.dataStore.columnIndexMapping.type] === 'P' &&
                        cityDetails[config.dataStore.columnIndexMapping.population] > 5000) {
                        this.createCityIndex(cityDetails)
                    }
                }
                this.initialized = true;
                logger.log('count: ' + this.count);
            })
            .catch((err) => {
                logger.error('Oops, there was en error loading city data', err);
                throw err;
            });
    }
}

module.exports = DataStoreService