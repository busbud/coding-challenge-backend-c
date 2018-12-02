const { server, parseGetParams, success, notFound } = require("./utils/server");
const Suggestions = require("./utils/suggestions");
const { suggestFromObjectList } = require("./utils/suggestions/fromList");

// globals
const PORT = process.env.PORT || 2345;
const INDEX_FILE = "./data/db.json";

// bootstrap the server
const DB = Suggestions.loadIndex(INDEX_FILE);

// not express :-)
const app = server();

app.get("/suggestions", (req, res) => {
  const params = parseGetParams(req.url);
  const results = suggestFromObjectList(DB, params.q);
  // no results there
  if (results.suggestions.length === 0) return notFound(res, results);
  // we got results yeah !
  return success(res, results);
});

const httpServer = app.listen(PORT, "0.0.0.0", () => {
  console.log("server listening in ", PORT);
});

module.exports = httpServer;
