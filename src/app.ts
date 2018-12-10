import http from "http";
import HttpStatus from "http-status-codes";
var port = process.env.PORT || 2345;

import FileAllCities from "./infrastructure/FileAllCities";
import AutoCompleteResultForCities from "./application/AutoCompleteResultForCities";

const autoCompleteResultForCities = new AutoCompleteResultForCities(
  new FileAllCities()
);

interface QueryParameters {
  q: string;
  latitude?: string;
  longitude?: string;
}

const getRequestParameters = (req): QueryParameters => {
  let q = req.url.split("?");
  let result: QueryParameters = { q: "" };

  if (q.length >= 2) {
    q[1].split("&").forEach(item => {
      try {
        result[item.split("=")[0]] = item.split("=")[1];
      } catch (e) {
        result[item.split("=")[0]] = "";
      }
    });
  }

  return result;
};

export default http
  .createServer(async function(req, res) {
    const queryParameter = getRequestParameters(req);

    if (req.method !== "GET") {
      res.writeHead(HttpStatus.NOT_IMPLEMENTED, {
        "Content-Type": "text/plain"
      });
      res.end();
    }

    if (req.url.indexOf("/suggestions") === 0) {
      const result = await autoCompleteResultForCities.proceed({
        name: queryParameter.q,
        longitude: Number(queryParameter.longitude),
        latitude: Number(queryParameter.latitude)
      });

      if (result.suggestions.length === 0) {
        res.writeHead(HttpStatus.NOT_FOUND, { "Content-Type": "text/plain" });
      } else {
        res.writeHead(HttpStatus.OK, { "Content-Type": "text/plain" });
      }

      res.end(JSON.stringify(result));
    } else {
      res.writeHead(HttpStatus.NOT_IMPLEMENTED, {
        "Content-Type": "text/plain"
      });
      res.end();
    }
  })
  .listen(port, () => "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
