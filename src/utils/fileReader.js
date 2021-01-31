/** File reading functions.
 * @module utils/readFile
 */
const fs = require("fs");

module.exports = {
  /**
   * Reads file for path.
   * @param {string} path - The path
   * @returns {string} File content
   */
  async readFile(path) {
    return new Promise((resolve, reject) => {
      fs.readFile(path, "utf-8", (err, fileContent) => {
        if (err) {
          reject(err);
        }
        resolve(fileContent);
      });
    });
  },
};
