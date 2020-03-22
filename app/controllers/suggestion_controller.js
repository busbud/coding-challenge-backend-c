var utils = require('../utils/utils');
var suggestionService = require('../services/suggestion_service');

exports.root = async function (req, res) {
    var { q: term, latitude: lat, longitude: lon } = req.query;
    var suggestions = await suggestionService.search(term, lat, lon);
    if (utils.isNotEmpty(suggestions)) {
        res.status(200).send({ suggestions: suggestions });
    }
    else {
        res.status(404).send({ suggestions: suggestions });
    }

}