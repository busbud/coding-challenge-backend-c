import "babel-polyfill";

import calculateScore from "./calculateScore";
import fetchData from "./dataFetcher";
import { cityUpdater, searchDatabase } from "./psql";

const createScoreFunction = params => async suggestion => {
    const score = await calculateScore(params, suggestion);
    return { score, ...suggestion };
}

const sortSuggestions = (a, b) => {
    if (a.score < b.score) return 1;
    if (a.score > b.score) return -1;
    return 0;
}

// The data will be stored on a postgresql database, both for performance reasons and for easier Heroku integration
const updateDatabase = async () => {
    const { updateLine, end } = await cityUpdater();
    fetchData(updateLine, end);
}

const search = async params => {
    const resultsWithoutScore = await searchDatabase(params.q);
    const assignScore = createScoreFunction(params);
    const resultsUnordered = await Promise.all(resultsWithoutScore.map(assignScore));
    const results = resultsUnordered.sort(sortSuggestions);
    return results;
}

module.exports = {
    updateDatabase,
    search,
}
