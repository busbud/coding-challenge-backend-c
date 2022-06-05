import { createServer } from "http";
import { parse } from "url";
import cache from "memory-cache";
import FileDatasource from "./data/file-datasource.js";
import Suggestions from "./services/suggestions.js";
import LRUCacheService from "./services/lru-cache-service.js";

const port = process.env.PORT || 2345;
const lruCache = new LRUCacheService();
const datasource = new FileDatasource(`data/cities_canada-usa.tsv`);
const suggestions = new Suggestions(datasource, lruCache);

export default createServer(async (req, res) => {
  await suggestions.initialize();
  if (req.url.indexOf("/suggestions") !== 0) {
    res.writeHead(404, { "Content-Type": "application/json" });
    res.end();
  }

  if (req.url.indexOf("/suggestions") === 0) {
    let result = [];
    const cachedResult = cache.get(req.url);

    if (cachedResult) {
      result = cachedResult;
    }

    const query = parse(req.url, true).query || {};

    if (query.q?.length < 3) {
      res.writeHead(404, { "Content-Type": "application/json" });
      result = JSON.stringify({
        suggestions: [],
      });
    } else {
      const data = await suggestions.searchData(
        query.q,
        query.latitude,
        query.longitude
      );

      if (data.length) {
        res.writeHead(200, { "Content-Type": "application/json" });

        if (data.length === 1) {
          if (!lruCache.isRecentlyUsed(data[0].name)) {
            lruCache.setItem(data[0].name, 1);
          }
        }

        result = JSON.stringify({
          suggestions: data,
        });

        if (data.length > 50) {
          cache.put(req.url, result);
        }
      } else {
        res.writeHead(404, { "Content-Type": "application/json" });
        result = JSON.stringify({
          suggestions: [],
        });
      }
    }
    res.write(result);
    res.end();
  }
}).listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
