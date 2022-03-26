// Keep them outside of the function scope so the regexes are kept in memory
const diacriticsRemovals = [
    [new RegExp(/[àáâãäå]/g), 'a'],
    [new RegExp(/æ/g), 'ae'],
    [new RegExp(/ç/g), 'c'],
    [new RegExp(/[èéêë]/g), 'e'],
    [new RegExp(/[ìíîï]/g), 'i'],
    [new RegExp(/ñ/g), 'n'],
    [new RegExp(/[òóôõö]/g), 'o'],
    [new RegExp(/œ/g), 'oe'],
    [new RegExp(/[ùúûü]/g), 'u'],
    [new RegExp(/[ýÿ]/g), 'y'],
];

const normalize = (text) => {
    text ||= '';

    return diacriticsRemovals.reduce(
        (result, replacementTuple) =>
            result.replace(replacementTuple[0], replacementTuple[1]),
        text.toLowerCase()
    );
};

module.exports.normalize = normalize;
