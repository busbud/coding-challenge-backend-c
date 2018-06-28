const express = require("express");
const app = express();
const http = require("http").Server(app);
const port = process.env.PORT || 2345;

const dbFile = __dirname + "/../data/cities_canada-usa.tsv";
const cityRepository = require("./infrastructure/cityRepository")({ dbFile });

if (process.env.NODE_ENV === "production") {
  require("./presentation/security")(app);
}

http.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});

require("./presentation/routes")(app, cityRepository);

module.exports = app;
