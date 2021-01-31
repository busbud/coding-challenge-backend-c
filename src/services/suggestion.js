/** Api bussiness logic
 * @module services/suggestion
 */
const db = require("../db");
const queries = require("../db/queries");

module.exports = {
  /**
   * Search given query againts locations table.
   * @param {string} q - The query
   * @returns {Array} Found rows
   */
  async searchQuery(q) {
    const result = await db.query(queries.searchWord, [q]);
    return result.rows;
  },
  /**
   * Search given query againts locations table by longitude&latitude.
   * @param {string} q - The query
   * @param {number} long - Longitude
   * @param {number} lat - Latitude
   * @returns {Array} Found rows
   */
  async searchQueryByLocation(q, long, lat) {
    const result = await db.query(queries.searchWordWithLocationScore, [q, long, lat]);
    return result.rows;
  },
};
