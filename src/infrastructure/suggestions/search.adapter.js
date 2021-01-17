const {index, client} = require('./elasticsearch.client')
const score = require('./score')

const fulltextSearch = async (term, lat, lon) => {
    return new Promise((resolve, reject) => {
        client.search({
            index: index,
            body: {}
        }).then((response) => {
            console.log(response)
            const parsed = parseResponse(response)
            console.log(parsed)
            resolve(parsed)
        })
            .catch(reason => {
                console.log(reason)
                reject(reason)
            })
    })
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
