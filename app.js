import { createServer } from "http";
import { parse } from "url";
var port = process.env.PORT || 2345;
import FileDatasource from "./data/file-datasource.js";
import Suggestions from "./services/suggestions.js";

const datasource = new FileDatasource(`data/cities_canada-usa.tsv`);
const suggestions = new Suggestions(datasource);
suggestions.initialize();

export default createServer(async (req, res) => {
  res.writeHead(404, { "Content-Type": "text/plain" });

  if (req.url.indexOf("/suggestions") === 0) {
    const query = parse(req.url, true).query || {};
    const data = await suggestions.searchData(
      query.q,
      query.latitude,
      query.longitude
    );

    res.end(
      JSON.stringify({
        suggestions: data,
      })
    );
  } else {
    res.end();
  }
}).listen(port, "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
