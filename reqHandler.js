function reqHandler() {
    this.cityData = [];

    return this
}

reqHandler.prototype.getSuggestions = function() {
    var self = this;

    return function(req, res, next) {
        console.log(self.cityData)
        res.status(404).send({"suggestions": []})
    }
}

module.exports = reqHandler;