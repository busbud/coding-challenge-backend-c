const adapter = require('../infrastructure/suggestions.adapter')

module.exports.search = async (term, lat, long) => {
    return adapter.fulltextSearch(term, lat, long)
}
