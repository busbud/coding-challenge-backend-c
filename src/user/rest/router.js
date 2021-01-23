const suggestionsController = require('./controller/suggestions.controller');
const maintenanceController = require('./controller/maintenance.controller');
const errorResponse = require('./response/error.response');

const router = async (req, res) => await suggestionsController(req, res)
    || await maintenanceController(req, res)
    || errorResponse.notFound(res);

module.exports = router;
