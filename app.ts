import { CitySuggestionService } from "./services/citySuggestionService";

const http = require("http");
const url = require("url");

const port = process.env.PORT || 2345;

module.exports = http
  .createServer(function(req, res) {
    const cityService = new CitySuggestionService();
    res.writeHead(404, { "Content-Type": "text/plain" });

    const requestUrl = url.parse(req.url, true);
    const { latitude, longitude } = requestUrl.query;
    const userLocation =
      latitude && longitude
        ? {
            latitude,
            longitude
          }
        : undefined;

    if (req.url.indexOf("/suggestions") === 0) {
      res.end(
        JSON.stringify({
          suggestions: cityService.getSuggestions(
            requestUrl.query.q,
            userLocation
          )
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
