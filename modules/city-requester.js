const database = require("./database");
const score = require("./score");

const MIN_CHARS = 2;
const MAX_ELEMENTS = 20;

module.exports = (() => {
    const sortSuggestionsByScore = suggestions => {
        suggestions.sort((element1, element2) => {
            if (element2.score > element1.score) {
                return 1;
            } else if (element2.score < element1.score) {
                return -1;
            }
            return 0;
        });
        return suggestions;
    };

    /*
    * Limit returned size
    * */
    const purgeData = suggestions => {
        //limit nb returned elements
        return suggestions.slice(0, MAX_ELEMENTS);
    };

    /*
    * Round float
    * */
    const roundScores = suggestions => {
        suggestions.forEach(element => {
            element.score = Math.round(element.score * 10) / 10;
        });
        return suggestions;
    };


    const getSuggestions = query => {
        return new Promise((resolve, reject) => {
            //some basic tests about query content and length
            if (!query || !query.destination) {
                reject({status: 400, error: "bad request, not destination provided"});
                return;
            } else if (query.destination.length < MIN_CHARS) {
                reject({status: 400, error: "bad request, please use at least two chars"});
            }
            const transformers = [score.scoreStringsByName(query.destination)];
            if (query.latitude && query.longitude) {
                transformers.push(score.setDistance(query.latitude, query.longitude));
            }
            database.queryData(query.destination, transformers)
                .then(score.scoreFromNameAndDistance)
                .then(sortSuggestionsByScore)
                .then(purgeData)
                .then(roundScores)
                .then(resolve)
                .catch((err) => {
                    reject({status: 500, error: err});
                });
        });
    };

    return {
        getSuggestions: getSuggestions
    };
})();
