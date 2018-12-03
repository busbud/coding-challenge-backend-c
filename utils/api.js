const rateLimit = require("express-rate-limit");

const { validateParamsMiddleware } = require("./suggestions");
const { suggestFromList } = require("./suggestions/fromList");
const { suggestFromIndex } = require("./suggestions/fromIndex");
const Cache = require("./cache");

/** the main process of computing suggestions */
function suggestionBaseEndpoint(DB, suggest, cache = true) {
  return (req, res) => {
    const params = validateParamsMiddleware(req);
    // params are not well formatted
    if (!params) return res.status(422).end('{error: "malformed request"}');
    // get the results

    const suggestOperation = suggest.bind(null, DB, params.query, params.pivot);
    let results;

    if (cache) {
      results = Cache.fromCacheOr(
        req.url,
        suggest.bind(null, DB, params.query, params.pivot)
      );
    } else {
      const json = suggestOperation();
      results = {
        json,
        serialized: JSON.stringify(json)
      };
    }

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

  app.get("/suggestions", suggestionBaseEndpoint(DB, suggestFromList), limiter);
  app.get("/suggestions-nolimit", suggestionBaseEndpoint(DB, suggestFromList));
  app.get(
    "/suggestions-nolimit-nocache",
    suggestionBaseEndpoint(DB, suggestFromList)
  );
  app.get("/suggestions-index", suggestionBaseEndpoint(DB, suggestFromIndex));
  app.get(
    "/suggestions-index-nocache",
    suggestionBaseEndpoint(DB, suggestFromIndex, false)
  );

  return app;
};
