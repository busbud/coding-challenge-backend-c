import * as stringSimilarity from "string-similarity"

export function calculateWordScore(queryString: string, resultString: string) {
    return stringSimilarity.compareTwoStrings(queryString.toLowerCase(), resultString.toLowerCase());
}
