const express = require('express');
const app = express();
const config = require('./config/config');
const http = require('http');

const DatabaseService = require('./services/data-store.service');
const CityService = require('./services/suggestions-engine.service');
const Logger = require('./utils/logger.service');

let memoryCache = {}; // todo-dhia: expire cache items


const databaseService = new DatabaseService();
let cityService;
const logger = new Logger();


app.post('*', (req, res, next) => {
    res.status(405).send('405 Method Not Allowed. To get started,  GET /suggestions');
});

//for loader.io load test
app.get(`/${config.loadTest.loaderIOValidationToken}`, (req, res, next) => {
    res.status(200).send(config.loadTest.loaderIOValidationToken);
});

app.get('/suggestions', async (req, res) => {

    const validations = validatePayload(req);
    if (validations.length > 0) {
        res.status(400).send(validations);
        return;
    }

    const suggestedCities = getCitySuggestions(req.query, req.url);
    if (req.query.q && suggestedCities.length == 0) { // non-existent city
        res.status(404);
    } else {
        res.status(200)
    }
    res.json({
        suggestions: suggestedCities
    });
})

app.get('*', (req, res, next) => {
    res.status(404).send('404 - To get started,  GET /suggestions');
});

app.use((err, req, res, next) => {
    logger.error(err);
    res.send(err.status || 500, 'Oops, there was an error processing you request. Please try again');

});


validatePayload = (req) => {
    let queryValidation = [];
    if (!req.query.q)
        queryValidation.push('Invalid q');

    if (typeof req.query.latitude !== 'undefined') {
        if (isNaN(req.query.latitude) || !(parsedLatitude = parseInt(req.query.latitude) <= 90 && parseInt(req.query.latitude) >= -90)) {
            queryValidation.push('Invalid latitude');
        }
    }

    if (typeof req.query.longitude !== 'undefined') {
        if (isNaN(req.query.longitude) || !(parseInt(req.query.longitude) <= 180 && parseInt(req.query.longitude) >= -180)) {
            queryValidation.push('Invalid latitude');
        }
    }
    return queryValidation;
}

/**
 * return city suggestions based on the query.
 * Will check first in the cache, if not found it will query the data store.
 * 
 * TODO: Currently, the cache is not LRU (least-recently-used). It will keep adding items undefinably. 
 * It needs to be changed to remove LRU items.
 */

getCitySuggestions = (query, url) => {

    let suggestedCities;
    if (memoryCache[url]) {
        suggestedCities = memoryCache[url];
    } else {
        suggestedCities = cityService.findCities(query.q, query.latitude, query.longitude)
        memoryCache[url] = suggestedCities;
    }
    return suggestedCities;
}

const server = http.Server(app);

//initialize data here before starting the service
function initializeData() {
    databaseService.initialize()
        .then(() => {
            cityService = new CityService(databaseService);
            logger.log('Data initialized...');
            server.listen(config.port, () => logger.log(`Server started at port:${config.port}`))
        });
}

initializeData();

module.exports = app;