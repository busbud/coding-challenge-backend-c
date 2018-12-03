const rateLimit = require("express-rate-limit");

const { validateParamsMiddleware } = require("./suggestions");
const { suggestFromList } = require("./suggestions/fromList");
const Cache = require("./cache");

/** the main process of computing suggestions */
function suggestionBaseEndpoint(DB) {
  return (req, res) => {
    const params = validateParamsMiddleware(req);
    // params are not well formatted
    if (!params) return res.status(422).end('{error: "malformed request"}');
    // get the results

    const results = Cache.fromCacheOr(
      req.url,
      suggestFromList.bind(null, DB, params.query, params.pivot)
    );

    // no results there
    if (results.json.suggestions.length === 0)
      return res.status(404).end(results.serialized);
    // we got results yeah !
    return res.end(results.serialized);
  };
}

module.exports = function(DB) {
  const app = require("express")();
  app.enable("trust proxy"); // only if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)

  // 50 req/seq from an ip address
  const limiter = rateLimit({
    windowMs: 3 * 1000, // 2 seconds@
    max: 500 // limit each IP to 100 requests per windowMs
  });

  app.get("/suggestions", suggestionBaseEndpoint(DB), limiter);
  app.get("/suggestions-nolimit", suggestionBaseEndpoint(DB));

  return app;
};
