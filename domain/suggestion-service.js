const repository = require('../repository/suggestion-repo');
const Error = require('./error/error');
const { makeSuggestion } = require('./suggestion-model');

module.exports.getSuggestions = async (query) => {

    let suggestions = await repository.getSuggestions(query.q, query.latitude, query.longitude);

    if (suggestions.length === 0) {
        throw new Error.NotFoundError();
    }

    return suggestions.map(item => {
        return makeSuggestion(
            `${item.name}, ${item.state}, ${item.country}`,
            item.latitude,
            item.longitude,
            calculateScore(item.longitude, item.latitude, query.longitude, query.latitude, item.name, query.q) || 1
        );
    }).sort((a, b) => a.score > b.score);
}

/**
 * 
 * @param {number} x1 
 * @param {number} y1 
 * @param {number} x2 
 * @param {number} y2 
 * @param {string} cityName 
 * @param {string} queryCityName
 */
const calculateScore = (x1, y1, x2, y2, cityName, queryCityName) => {
    
    let dist = Math.hypot(x1 - x2, y1 - y2);

    const nameScoreWeight = (cityName.length - queryCityName.length);

    const score = 1 - (dist / (20 - nameScoreWeight));

    if (score < 0) return 0.1;

    return score.toFixed(1);
}
