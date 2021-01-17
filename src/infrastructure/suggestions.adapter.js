const elasticSearchConfig = require('../../config').elasticSearch
const {Client} = require('@elastic/elasticsearch')
const client = new Client({node: elasticSearchConfig.baseUrl})

const fulltextSearch = async (term, lat, lon) => {
    return new Promise((resolve, reject) => {
        client.search({
            index: elasticSearchConfig.index,
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
    const minScore = Math.min(...suggestions.map(suggestion => suggestion._score))

    console.log('maxScore', maxScore)
    console.log('minScore', minScore)

    return suggestions.map(suggestion => {
        return {
            id: suggestion._id,
            score: calculateScore(minScore, maxScore, suggestion._score),
            ...suggestion._source
        }
    });
}

const calculateScore = (minScore, maxScore, score) => {
    console.log(minScore, maxScore, score)
    return (minScore - score) / (minScore - maxScore)
}

exports.fulltextSearch = fulltextSearch
