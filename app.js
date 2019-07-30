const createRedisClient = require("./src/cache").createClient;
const http = require("http");
const port = process.env.PORT || 2345;
const url = require("url");

const dataUtils = require("./src/suggestions/dataUtils");
const connectMongo = require("./src/mongo").connect;
const { getErrorMsg } = require("./src/apiUtils");
const contentType = { "Content-Type": "application/json" };

createRedisClient();
connectMongo();

module.exports = http
  .createServer(function(req, res) {
    const { query, pathname } = url.parse(req.url, true);
    if (pathname.replace(/\/+$/, "") === "/suggestions") {      
      dataUtils
        .getSuggestions(query)
        .then(suggestions => {
          const statusCode = suggestions.length === 0 ? 404 : 200;
          res.writeHead(statusCode, contentType);
          res.end(
            JSON.stringify({
              suggestions
            })
          );
        })
        .catch(err => {
          res.writeHead(500, contentType);
          res.end(JSON.stringify(getErrorMsg(err)));
        });
    } else {
      res.writeHead(404, contentType);
      res.end(JSON.stringify({ message: "Not Found" }));
    }
  })
  .listen(port);

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
