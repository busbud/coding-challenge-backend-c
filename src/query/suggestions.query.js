const adapter = require('../infrastructure/suggestions/search.adapter')

module.exports.search = async (q, latitude, longitude) => {
    return adapter.fulltextSearch(q, latitude, longitude)
}
