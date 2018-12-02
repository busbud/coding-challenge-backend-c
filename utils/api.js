const app = require("express")();
const { suggestFromObjectList } = require("./suggestions/fromList");

app.get("/suggestions", (req, res) => {
  console.log("suggestions");
  const params = parseGetParams(req.url);
  const results = suggestFromObjectList(DB, params.q);
  // no results there
  console.log("FOUND", results);
  if (results.suggestions.length === 0) return notFound(res, results);
  // we got results yeah !
  return success(res, results);
});

module.exports = {
  app
};
