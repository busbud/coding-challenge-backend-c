const rateLimit = require("express-rate-limit");

const { validateParamsMiddleware } = require("./suggestions");
const { suggestFromList } = require("./suggestions/fromList");
const { withCode } = require("./server");
const Cache = require("./cache");

module.exports = function(DB) {
  const app = require("express")();
  app.enable("trust proxy"); // only if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)

  // 50 req/seq from an ip address
  const limiter = rateLimit({
    windowMs: 3 * 1000, // 2 minutes
    max: 500 // limit each IP to 100 requests per windowMs
  });

  app.get("/suggestions", (req, res) => {
    const params = validateParamsMiddleware(req);
    // params are not well formatted
    if (!params) return res.status(422).end('{error: "malformed request"}');
    // get the results

    const results = suggestFromList(DB, params.query, params.pivot);
    // in json
    const resultsJSON = JSON.stringify(results);
    // no results there
    if (results.suggestions.length === 0)
      return res.status(404).end(resultsJSON);
    // we got results yeah !
    return res.end(resultsJSON);
  });

  return app;
};
