const { curry } = require("lodash");

/** a standard tsv limiter */
const TSV_DELIMITER = /[ \s]+/;

/** transform a tsv line to object with each key mapping the header name */
const lineToMap = curry((headersArray, line) => {
  const splitted = line.split(TSV_DELIMITER);
  return headersArray.reduce((object, headerLabel, index) => {
    object[headerLabel] = splitted[index];
    return object;
  }, {});
});

module.exports = {
  lineToMap
};
