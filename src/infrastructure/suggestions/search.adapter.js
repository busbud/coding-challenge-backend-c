const {index, client} = require('./elasticsearch.client')
const score = require('./score')

const fulltextSearch = async (q, latitude, longitude) => {
    return new Promise((resolve, reject) => {
        client.search({
            index: index,
            body: buildRequest(q, latitude, longitude)
        }).then((response) => {
            const parsed = parseResponse(response)
            resolve(parsed)
        })
            .catch(reason => {
                console.log(reason)
                reject(reason)
            })
    })
}

const buildRequest = (q, latitude, longitude) => {
    let functions = []
    console.log(latitude, longitude)
    if (latitude !== null && longitude !== null) {
        functions.push({
            gauss: {
                location: {
                    origin: {
                        "lat": latitude,
                        "lon": longitude
                    },
                    scale: "8000km"
                }
            }
        })
    }

    return {
        query: {
            function_score: {
                query: {
                    bool: {
                        must: [

                            {
                                match_bool_prefix: {
                                    fullSuggestion: {
                                        query: q,
                                        analyzer: "standard"
                                    }
                                }
                            }
                        ]
                    }
                },
                functions: functions
            }
        }
    }
}

const parseResponse = (response) => {
    const suggestions = response.body.hits.hits;
    if (suggestions.length === 0) {
        return []
    }

    const maxScore = response.body.hits.max_score;
    const minScore = score.minScore(suggestions)

    console.log('maxScore', maxScore)
    console.log('minScore', minScore)

    return suggestions.map(suggestion => {
        return {
            id: suggestion._id,
            score: score.calculate(minScore, maxScore, suggestion._score),
            ...suggestion._source
        }
    });
}

exports.fulltextSearch = fulltextSearch
