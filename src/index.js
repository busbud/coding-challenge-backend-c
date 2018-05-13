import "babel-polyfill";

import calculateScore from "./calculateScore";
import fetchData from "./dataFetcher";
import { databaseUpdater, searchDatabase } from "./psql";

const createScoreFunction = params => async suggestion => {
    const score = await calculateScore(params, suggestion);
    return { score, ...suggestion };
}

const sortSuggestions = (a, b) => {
    if (a.score < b.score) return 1;
    if (a.score > b.score) return -1;
    return 0;
}

const updateDatabase = async () => {
    const { updateLine, end } = await databaseUpdater();
    fetchData(updateLine, end);
}

const search = async params => {
    try {
        const resultsWithoutScore = await searchDatabase(params.q);
        const assignScore = createScoreFunction(params);
        const resultsUnordered = await Promise.all(resultsWithoutScore.map(assignScore));
        const results = resultsUnordered.sort(sortSuggestions);
        return results;
    } catch (err) {
        console.log("Error in search method:", err);
        return [];
    }
}

module.exports = {
    updateDatabase,
    search,
}
