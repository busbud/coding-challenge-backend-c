const querystring = require('querystring');
const query = require('../../../query/suggestions.query');
const suggestionResponse = require('../response/suggestion.response');
const errorResponse = require('../response/error.response');

const handleQueryParameters = (req) => {
  const rawQueryString = req.url.split('?').slice(-1)[0];
  const params = querystring.parse(rawQueryString);

  return {
    q: params.q || null,
    latitude: params.latitude || null,
    longitude: params.longitude || null,
  };
};

const handler = async (req, res) => {
  if (req.url.indexOf('/suggestions') !== 0) {
    return false;
  }

  const { q, latitude, longitude } = handleQueryParameters(req);
  if (q === null) {
    errorResponse.badRequest(res, "Querystring 'q' must be informed.");
    return true;
  }

  query.search(q, latitude, longitude)
    .then((results) => suggestionResponse.success(res, results))
    .catch((reason) => errorResponse.internalError(res, reason));
  return true;
};

module.exports = handler;
