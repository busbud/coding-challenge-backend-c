/** Tsv file functions.
 * @module utils/tsvJSON
 */
module.exports = {
  /**
   * Converts tsv string to json.
   * @param {string} path - The tsv string
   * @returns {JSON} Converted json
   */
  tsv2JSON(tsv) {
    const lines = tsv.split("\n");
    const headers = lines.shift().split("\t");
    return lines.map((line) => {
      const data = line.split("\t");
      return headers.reduce((row, key, idx) => {
        row[key] = data[idx];
        return row;
      }, {});
    });
  },
};
