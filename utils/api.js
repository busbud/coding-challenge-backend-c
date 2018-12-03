const rateLimit = require("express-rate-limit");
const app = require("express")();
const { suggestFromObjectList } = require("./suggestions/fromList");

app.enable("trust proxy"); // only if you're behind a reverse proxy (Heroku, Bluemix, AWS ELB, Nginx, etc)

// 50 req/seq from an ip address
const limiter = rateLimit({
  windowMs: 1000, // 15 minutes
  max: 50 // limit each IP to 100 requests per windowMs
});

app.get(
  "/suggestions",
  (req, res) => {
    console.log("suggestions");
    const params = parseGetParams(req.url);
    const results = suggestFromObjectList(DB, params.q);
    // no results there
    if (results.suggestions.length === 0) return notFound(res, results);
    // we got results yeah !
    return success(res, results);
  },
  limiter
);

module.exports = {
  app
};
