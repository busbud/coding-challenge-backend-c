var suggestionService = require('../services/suggestion_service');

exports.root = async function (req, res, index) {
    var { q: term, latitude: lat, longitude: lon } = req.query;
    var results = await suggestionService.search(term, lat, lon);
    res.status(200).send({ suggestions: results });
}