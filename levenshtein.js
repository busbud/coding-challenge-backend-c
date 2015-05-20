//levenshtein distance as shown on http://en.wikipedia.org/wiki/Levenshtein_distance
//using Wagner-Fischer matrix http://en.wikipedia.org/wiki/Wagner-Fischer_algorithm
function levenshteinDistance(source, target) {
    //base cases
    if (source == target)
        return 0;
    if (source.length == 0)
        return target.length;
    if (target.length == 0)
        return source.length;

    //create matrix
    var wfMatrix = [];

    //iteration vars
    var row = 0;
    var col = 0;

    //initialize first column
    for (row = 0; row <= target.length; row++)
        wfMatrix[row] = [row];

    //initilize first row
    row = 0
    for (col = 0; col <= source.length; col++)
        wfMatrix[row][col] = col;

    //compute matrix based on min or diagonal values
    for (row = 1; row <= target.length; row++) {
        for (col = 1; col <= source.length; col++) {
            if (target.charAt(row - 1) === source.charAt(col - 1)) //character is the same use, diagonal value
                wfMatrix[row][col] = wfMatrix[row - 1][col - 1];
            else { //calculate min of (left value, top value, diagonal value) + 1
                wfMatrix[row][col] = Math.min(
                                        wfMatrix[row - 1][col - 1], //diagonal: substitution operation
                                        wfMatrix[row - 1][col], //left: deletion operation
                                        wfMatrix[row][col - 1] //top: insertion operation
                                     ) + 1;
            }
        }
    }

    //matrix filled, levenshtein distance in last index
    return wfMatrix[target.length][source.length];
}

module.exports = levenshteinDistance;