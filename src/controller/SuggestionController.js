const Suggestion = require("../services/SuggestionService.js");

/**
 * @description controller class of the suggestion api
 */
class SuggestionController {
  constructor() {}

  /**
   * @method search
   * @type GET
   * @description api controller calls search service
   * @param {ExpressRequest} req
   * @param {ExpressResponse} res
   */
  search(req, res) {
    const requestQuery = req.query;
    const { q, latitude, longitude } = requestQuery;

    if (!q || q.length == 0) res.json({});

    Suggestion.search({ query: q, latitude, longitude })
      .then((response) => {
        const { data, error } = response;
        if (error) {
          res.status(error.statusCode);
        }
        res.json(data);
      })
      .catch((err) => {
        res.send(err);
      });
  }

  /**
   * @method add
   * @type POST
   * @description api controller calls add service
   * @param {ExpressRequest} req
   * @param {ExpressResponse} res
   */
  add(req, res) {
    const body = req.body;
    Suggestion.add(body)
      .then((data) => res.json(data))
      .catch((error) => {
        res.status(error.statusCode);
        res.send(error);
      });
  }
}

module.exports = new SuggestionController();
