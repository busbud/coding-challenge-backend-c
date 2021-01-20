const suggestionsController = require('./controller/suggestions.controller')

const router = async (req, res) => {
    await suggestionsController(req, res) ||
    res.end();
}

module.exports = router
