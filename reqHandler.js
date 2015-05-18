function getSuggestions(req, res, next) {
    res.status(404).send({"suggestions": []})
}

module.exports = {
    getSuggestions: getSuggestions
}