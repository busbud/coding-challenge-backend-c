const express = require("express");
const app = express();
const http = require("http").Server(app);
const port = process.env.PORT || 2345;

http.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});

require("./routes")(app);

module.exports = app;
