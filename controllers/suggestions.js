const url = require('url');

// Pass request and response
let handleRequest = (req, res, data) => {

    let queryParams = getQueryStringParams(req);

    let results = data.filter(city => {
        return city.name.toLowerCase() == queryParams.q.toLowerCase()
    });

    res.end(JSON.stringify({
        suggestions: results
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

