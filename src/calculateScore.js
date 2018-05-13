// calculateScore is a pure function. The score of a suggestion does not depend on other suggestions, which allows for simpler logic
const calculateScore = (params, suggestion) => {
    if (!params.latitude || !params.longitude) {
        // If no position is specified, we'll first show the cities starting with the query, then those that countain the query
        // Bigger cities will be displayed first, as they are more likely to be searched
        const index = suggestion.name.indexOf(params.q); // Always a positive integer, otherwise the suggestion wouldnt have been returned by psql
        return (index === 0 ? 0.5 : 0) + suggestion.population / 50000000;
        // To ensure the score never exceeds 1, we could use the double of the most populated city instead of 50000000
        // However, this quick solution will yield the same results and won't cause a score above 1 for long enough
    } else {
        const distance = Math.hypot(parseFloat(params.latitude) - suggestion.latitude, parseFloat(params.longitude) - suggestion.longitude);
        if (distance === 0) return 1;
        const index = suggestion.name.indexOf(params.q);
        return ( index === 0 ? 0.5 : 0 ) + 1 / (distance + 2);
    }
}

// Even though calculateScore is a purely sync function, we are making it async
// This is because upon a search, a single calculateScore function can be called a huge amount of times
// Making it async will avoid blocking the main thread for a long period of time
// Once async, it can also bo moved to a different process or a different server
// that will be dedicated to calculating the score
const calculateScoreAsync = (params, suggestions) => {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve(calculateScore(params, suggestions));
        }, 0);
    });
}

export default calculateScoreAsync;
