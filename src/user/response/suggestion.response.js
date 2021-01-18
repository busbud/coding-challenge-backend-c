const of = (suggestion) => {
    return {
        name: suggestion.fullSuggestion,
        latitude: suggestion.location.lat,
        longitude: suggestion.location.lon,
        score: suggestion.score.toFixed(1)
    }
}

const fromList = (suggestions) => suggestions.map((suggestion) => of(suggestion))

const success = (res, suggestions) => {
    res.writeHead(200, {'Content-Type': 'application/json'});
    res.end(JSON.stringify({
        suggestions: fromList(suggestions)
    }));
}

module.exports.success = success
