const elastic = require('../../modules/elasticsearch');
var Bottleneck = require("bottleneck");

const get_suggestions = async (req, res) => {
    try {
        //Limit to 10 request a second
        const limiter = new Bottleneck({
            minTime: 100
        });

        const query = build_query(req);

        if (query != null) {
            const result = await limiter.schedule(() => { return elastic.get_suggestions(query) });
            if (result.length == 0) {
                res.status(404);
            }
            res.send({ suggestions: result });

        } else {
            res.status(400);
            res.send([]);
        }
    }
    catch (e) {
        console.log(e);
        res.status(500);
        res.send(e);
    }
};

function build_query(req) {
    const query = req.query['q'];
    const latitude = req.query['latitude'];
    const longitude = req.query['longitude'];

    if (query == null) return null;

    if (latitude != null && longitude != null && query != null) {
        //Send a geo query to elasticsearch with a wildcard string search
        return {
            index: "population",
            body: {
                query: {
                    function_score: {
                        query: {
                            wildcard: {
                                ascii: {
                                    value: "*" + query.toLowerCase() + "*",
                                    rewrite: "scoring_boolean"
                                }
                            }
                        },
                        functions: [
                            {
                                gauss: {
                                    location: {
                                        origin: { lat: latitude, lon: longitude },
                                        scale: "100km"
                                    }
                                }
                            }
                        ]
                    }
                }
            }
        }
    } else {
        //Send a regular wildcard query to elasticsearch
        return {
            index: "population",
            body: {
                query: {
                    wildcard: {
                        ascii: {
                            value: "*" + query.toLowerCase() + "*",
                            rewrite: "scoring_boolean"
                        }
                    }
                }
            }
        }
    }

    return null;
};

module.exports = {
    get_suggestions: get_suggestions
}