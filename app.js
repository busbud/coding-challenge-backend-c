const Suggestions = require("./utils/suggestions");
const { app } = require("./utils/api");

// globals
const PORT = process.env.PORT || 2345;
const ADDRESS = process.env.URL || "0.0.0.0";
const INDEX_FILE = "./data/db.json";

// bootstrap the server
const DB = Suggestions.loadIndex(INDEX_FILE);
const APP = app.listen(PORT, ADDRESS, () => {
  console.log("server listening in ", `${ADDRESS}:${PORT}`);
});

module.exports = APP;
