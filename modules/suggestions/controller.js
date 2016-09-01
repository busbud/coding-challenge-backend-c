'use strict';

var suggestionsController = class SuggestionsController {

    get(req, res) {

        return res.json({
            suggestions : []
        })

    }
};

module.exports = new suggestionsController();