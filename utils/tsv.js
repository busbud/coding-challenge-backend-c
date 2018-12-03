const { curry } = require("lodash");

/** a standard tsv limiter */
const TSV_DELIMITER = /\t/;

/** transform a tsv line to object with each key mapping the header name */
const lineToMap = curry(
  (delimiter, heuristicsTransform, headersArray, line) => {
    const splitted = heuristicsTransform(line).split(delimiter);
    return headersArray.reduce((object, headerLabel, index) => {
      object[headerLabel] = splitted[index];
      return object;
    }, {});
  }
);

const tsvLineToMap = lineToMap(TSV_DELIMITER);

module.exports = {
  lineToMap,
  tsvLineToMap
};
