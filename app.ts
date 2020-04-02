import { CityService } from "./services/cityService";

const http = require("http");
const url = require("url");

const port = process.env.PORT || 2345;

module.exports = http
  .createServer(function(req, res) {
    const cityService = new CityService();
    res.writeHead(404, { "Content-Type": "text/plain" });
    const requestUrl = url.parse(req.url, true);

    if (req.url.indexOf("/suggestions") === 0) {
      res.end(
        JSON.stringify({
          suggestions: cityService.getByName(requestUrl.query.q)
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
