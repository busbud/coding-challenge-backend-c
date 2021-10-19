// Calculates the similiraty score based on the string-similarity library (https://www.npmjs.com/package/string-similarity)
// It uses the Sørensen–Dice (https://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient) coefficient formula 
//to a score-like result between two strings

import * as stringSimilarity from "string-similarity"

export function calculateWordScore(queryString: string, resultString: string) {
    return stringSimilarity.compareTwoStrings(queryString.toLowerCase(), resultString.toLowerCase());
}
