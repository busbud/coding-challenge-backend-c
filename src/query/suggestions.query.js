const adapter = require('../infrastructure/suggestions/search.adapter')

module.exports.search = async (term, lat, long) => {
    return adapter.fulltextSearch(term, lat, long)
}
