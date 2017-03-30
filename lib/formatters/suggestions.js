function SuggestionsFormatter() {
    this.esToHttpOutput = function(input) {

        return {
            suggestions : input.hits.hits.map(function(hit) {
                var item = hit._source;

                return {
                    name: item.asciiName + ", " + item.admin1Code + ", " + item.countryCode,
                    latitude: item.location.lat,
                    longitude: item.location.lon,
                    score: hit._score / input.hits.max_score
                };
            })
        }
    }

    return this;
}

module.exports = SuggestionsFormatter;