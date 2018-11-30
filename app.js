const server = require("./utils/server").server;
const url = require("url");
const Suggestions = require("./utils/suggestions");
// globals
const PORT = process.env.PORT || 2345;
const INDEX_FILE = "./index.db";

// bootstrap the server
Suggestions.loadIndex(INDEX_FILE);

// not express :-)
const app = server();

app.get("/suggestions", (req, res) => {
  const query = url.parse(req.url);
  console.log(query.search);
  res.end(JSON.stringify(Suggestions.suggest("blainville")));
});

app.listen(PORT, "0.0.0.0", () => {
  console.log("server listening in ", PORT);
});
