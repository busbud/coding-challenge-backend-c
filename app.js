import { createServer } from "http";
import { parse } from "url";
import cache from "memory-cache";
import FileDatasource from "./data/file-datasource.js";
import Suggestions from "./services/suggestions.js";
import LRUCacheService from "./services/lru-cache-service.js";

const port = process.env.PORT || 2345;

const datasource = new FileDatasource(`data/cities_canada-usa.tsv`);
const lruCache = new LRUCacheService();
const suggestions = new Suggestions(datasource, lruCache);
suggestions.initialize();

export default createServer(async (req, res) => {
  if (req.url.indexOf("/suggestions") === 0) {
    res.writeHead(200, { "Content-Type": "application/json" });

    const cachedResult = cache.get(req.url);

    if (cachedResult) {
      res.end(cachedResult);
      return;
    }

    const query = parse(req.url, true).query || {};

    if (query.q?.length < 3) {
      res.end(
        JSON.stringify({
          suggestions: [],
        })
      );
      return;
    }
    const data = await suggestions.searchData(
      query.q,
      query.latitude,
      query.longitude
    );

    if (data.length === 1) {
      if (!lruCache.isRecentlyUsed(data[0].city)) {
        lruCache.setItem(data[0].city, 1);
      }
    }

    const result = JSON.stringify({
      suggestions: data,
    });

    if (data.length > 50) {
      cache.put(req.url, result);
    }
    res.end(result);
  } else {
    res.end();
  }
}).listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
