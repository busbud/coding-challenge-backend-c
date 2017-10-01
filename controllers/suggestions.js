const url = require('url');
const cityModel = require('./../models/city');

// Pass request and response
function handleRequest(req, res) {

    let queryParams = getQueryStringParams(req);
    let results = [];

    if(queryParams.q) {
        let search = queryParams.q;
        cityModel.find({ name: new RegExp(`^${search}`, 'i') }).then(cities => {
            results = cities;

            // Check is search was made in the past
            // Could have an expirationDate to avoid keeping the data for too long
            // CREATE new result for later request
        

            return returnResponse(res, results);
        })
        

    }
    else {
        returnResponse(res, results);        
    }


}

function returnResponse(res, results) {
    return res.end(JSON.stringify({
        suggestions: results
    }));
}

// Returns query params
function getQueryStringParams(req) {
    return url.parse(req.url, true).query;
}

function hasLocation(queryParams) {
    return queryParams.latitude && queryParams.longitude ? true : false;
}

module.exports = handleRequest;

