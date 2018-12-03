const Suggestions = require("./utils/suggestions");
const api = require("./utils/api");
const DB = require("./data/db.json");

// globals
const PORT = process.env.PORT || 2345;
const ADDRESS = process.env.URL || "0.0.0.0";
const INDEX_FILE = "./data/db.json";

// bootstrap the server

const API = api(DB);
API.listen(PORT, ADDRESS, () => {
  console.log("server listening in ", `${ADDRESS}:${PORT}`);
});

module.exports = API;
