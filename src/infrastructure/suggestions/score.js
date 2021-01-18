const calculateScore = (minScore, maxScore, score) => (minScore - score) / (minScore - maxScore)

const minScore = (suggestions) => Math.min(...suggestions.map(suggestion => suggestion._score))

module.exports.calculate = calculateScore
module.exports.minScore = minScore
