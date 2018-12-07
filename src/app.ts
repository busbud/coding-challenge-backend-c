import http from "http";
var port = process.env.PORT || 2345;

import FileAllCities from "./infrastructure/FileAllCities";
import GetAutoCompleteResultForCities from "./application/GetAutoCompleteResultForCities";

const getAutoCompleteResultForCities = new GetAutoCompleteResultForCities(
  new FileAllCities()
);

interface QueryParameter {
  q: string;
  latitude?: string;
  longitude?: string;
}

const requestParameters = (req): QueryParameter => {
  let q = req.url.split("?");
  let result: QueryParameter = { q: "" };

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
    const queryParameter = requestParameters(req);

    res.writeHead(404, { "Content-Type": "text/plain" });

    if (req.url.indexOf("/suggestions") === 0) {
      const result = await getAutoCompleteResultForCities.execute({
        name: queryParameter.q,
        longitude: Number(queryParameter.longitude),
        latitude: Number(queryParameter.latitude)
      });

      res.end(
        JSON.stringify({
          result
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port, () => "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
