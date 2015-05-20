function reqHandler() {
    this.db;

    return this
}

reqHandler.prototype.getSuggestions = function() {
    var self = this;

    return function(req, res, next) {

        var cityTerm    = req.query.q;
        var lat         = req.query.latitude;
        var long        = req.query.longitude;
        var suggestions = [];

        if (cityTerm) //perform search only if a term is provided
            suggestions = self.db.search(cityTerm, lat, long);

        if (suggestions.length > 0) {
            res.status(200).send({"suggestions": suggestions})
        } else {
            res.status(404).send({"suggestions": []})
        }
    }
}

module.exports = reqHandler;