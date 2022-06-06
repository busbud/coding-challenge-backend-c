import { createServer } from "http";
import { parse } from "url";
import cache from "memory-cache";
import FileDatasource from "./data/file-datasource.js";
import LRUCacheService from "./services/lru-cache-service.js";
import SuggestionsService from "./services/suggestions-service.js";

const port = process.env.PORT || 5000;
const lruCache = new LRUCacheService();
const datasource = new FileDatasource(`data/cities_canada-usa.tsv`);
const suggestions = new SuggestionsService(datasource, lruCache);

export default createServer(async (req, res) => {
  // Initialize the suggestions indexing
  await suggestions.initialize();

  if (req.url.indexOf("/suggestions") !== 0) {
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end();
  }

  if (req.url.indexOf("/suggestions") === 0) {
    const cachedResult = cache.get(req.url);

    if (cachedResult) {
      res.end(
        JSON.stringify({
          suggestions: cachedResult,
        })
      );
      return;
    }

    const query = parse(req.url, true).query || {};

    const data = await suggestions.searchData(
      query.q,
      query.latitude,
      query.longitude
    );

    if (!data.length) {
      res.writeHead(404, { "Content-Type": "application/json" });
      res.end(
        JSON.stringify({
          suggestions: [],
        })
      );
      return;
    }

    res.writeHead(200, { "Content-Type": "application/json" });

    const result = JSON.stringify({
      suggestions: data,
    });

    if (data.length > 50) {
      cache.put(req.url, result);
    }
    res.end(result);
  }
}).listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
