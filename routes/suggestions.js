var express = require('express');
var router = express.Router();

function SuggestionsRouter(dependencies) {
    this.router = router;

    if(!dependencies.suggestionsController) {
        throw new Error("Missing injected dependency suggestionsController");
    }

    if(!dependencies.suggestionsValidator) {
        throw new Error("Missing injected dependency suggestionsValidator");
    }

    this.router.get('/', [
        dependencies.suggestionsValidator.validateQueryString,
        dependencies.suggestionsController.searchLocations
    ]);

    return this;    
}

module.exports = SuggestionsRouter;