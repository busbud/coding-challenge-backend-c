const config = require("config");
const loaders = require("./src/loaders");
const express = require("express");
const url = config.get("server.ip");
const port = config.get("server.port");

const app = express();

const start = async () => {
  await loaders(app);

  app.listen(port, async () => {
    console.log("Server running at http://127.0.0.1:%d/suggestions", port);
  });
  return app;
};
start();

module.exports = app;
