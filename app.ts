import { CitySuggestionService } from "./services/citySuggestionService";

const http = require("http");
const url = require("url");

const port = process.env.PORT || 2345;

module.exports = http
  .createServer(function(req, res) {
    if (req.url.indexOf("/suggestions") === 0) {
      handleSuggestionsRequest(req, res);
    } else {
      handleNonExistentEndpoint(res);
    }
  })
  .listen(port, "127.0.0.1");

function handleSuggestionsRequest(req, res) {
  const { q, userLocation } = parseSuggestionsParams(req);

  try {
    const suggestions = CitySuggestionService.getSuggestions(q, userLocation);
    res.writeHead(suggestions.length > 0 ? 200 : 404, {
      "Content-Type": "application/json"
    });
    res.end(
      JSON.stringify({
        suggestions
      })
    );
  } catch (e) {
    returnApiError(res, 500, { message: e.message, stack: e.stack });
  }
}

function parseSuggestionsParams(req) {
  const requestUrl = url.parse(req.url, true);
  const { latitude, longitude, q } = requestUrl.query;
  const userLocation =
    latitude && longitude
      ? {
          latitude,
          longitude
        }
      : undefined;

  return { q, userLocation };
}

function handleNonExistentEndpoint(res) {
  returnApiError(res, 404, { message: "The endpoint was not found." });
}

function returnApiError(res, statusCode: number, error: any) {
  res.writeHead(statusCode, { "Content-Type": "application/json" });
  res.end(JSON.stringify(error));
}

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
