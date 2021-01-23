const querystring = require('querystring');
const query = require('../../../query/suggestions.query');
const suggestionResponse = require('../response/suggestion.response');
const errorResponse = require('../response/error.response');

const handler = async (req, res) => {
  if (req.url.indexOf('/suggestions') !== 0) {
    return false;
  }

  const rawQueryString = req.url.split('?').slice(-1)[0];
  const params = querystring.parse(rawQueryString);
  if (params.q === undefined) {
    errorResponse.badRequest(res, "Querystring 'q' must be informed.");
    return true;
  }
  const { q } = params;
  const latitude = params.latitude || null;
  const longitude = params.longitude || null;
  query.search(q, latitude, longitude)
    .then((results) => suggestionResponse.success(res, results))
    .catch((reason) => errorResponse.internalError(res, reason));
  return true;
};

module.exports = handler;
