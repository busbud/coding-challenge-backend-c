const suggestionsController = require('./controller/suggestions.controller')
const maintenanceController = require('./controller/maintenance.controller')

const router = async (req, res) => {
    await suggestionsController(req, res) ||
    await maintenanceController(req, res)
    res.end();
}

module.exports = router
