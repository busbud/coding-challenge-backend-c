const City = require("../models/City.js");
const Score = require("../../lib/score.js");

/**
 * @description service class of the suggestion api
 */
class SuggestionService {
  constructor() {}

  /**
   * @method search
   * @description find cities with given query
   * @param {{query: String, latitude: Number, longitude: Number}} options
   * @returns {Object}
   */
  async search({ query, latitude, longitude }) {
    try {
      const responseBody = { suggestions: [] };
      let error;
      const results = await City.find(
        { name: { $regex: `.*${query}.*` }, population: { $gte: 5000 } },
        { name: 1, latitude: 1, longitude: 1, _id: 0 }
      ).lean();

      const hasResults = results && results.length > 0;
      const hasLocation = latitude && longitude;

      if (hasResults && hasLocation) {
        const resultsWithScores = Score.getResultsWithScores(
          results,
          latitude,
          longitude
        );
        responseBody.suggestions = resultsWithScores;
      } else if (!hasResults) {
        error = new Error(`No fitting results with the query "${query}"`);
        error.statusCode = 404;
      } else {
        responseBody.suggestions = results;
      }
      return { data: responseBody, error };
    } catch (err) {
      throw err;
    }
  }

  /**
   * @method add
   * @description add new city to db
   * @param {String} city
   */
  async add(city) {
    try {
      const model = new City(city);
      await model.save();
    } catch (error) {
      error.statusCode = 400;
      throw error;
    }
  }
}

module.exports = new SuggestionService();
