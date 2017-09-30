const url = require('url');
let previousQueries = [];

// Pass request and response
let handleRequest = (req, res, data) => {

    let queryParams = getQueryStringParams(req);
    let search = queryParams.q.toLowerCase();

    // Check is search was made in the past
    // Could have an expirationDate to avoid keeping the data for too long
    let previousResults = previousQueries[search];

    // If no search in the past
    if(!previousResults) {
        results = data.filter(city => {
            // @todo : pattern / Regex instead
            return city.name.toLowerCase() == queryParams.q.toLowerCase()
        });

        previousQueries[search] = results;
    }

    res.end(JSON.stringify({
        suggestions: previousResults || results
    }));
}

// Returns query params
let getQueryStringParams = (req) => {
    return url.parse(req.url, true).query;
}

let hasLocation = queryParams => {
    return queryParams.latitude && queryParams.longitude ? true : false;
}

module.exports = handleRequest;

