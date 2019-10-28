const search = require('../components/search');

/*
    handles routing to appropriate hadnlers based on endpoint

    @param req - http request object
    @param res - http response object
*/
function routes(req, res) {
    // centralize the setting of response heders and packaging of response data
    let _routeCallback = (status, data, error) => {
        res.writeHead(status, {'Content-Type': 'application/json'});
        if(error) {
            res.end(JSON.stringify({'message': error.message}));
        } else {
            res.end(JSON.stringify(data));
        }
    }

    // extract the relevant information from the request object and pass
    // to the inner components for use. This reduces the reliance of inner
    // components on the http server objects
    let contextObject = {
        'url': req.url
    }

    if (req.url.indexOf('/suggestions') === 0) {
        search.suggestions(contextObject, _routeCallback);
    } else {
        res.writeHead(404, {'Content-Type': 'application/json'});
        res.end();
    }
}

module.exports = routes;
