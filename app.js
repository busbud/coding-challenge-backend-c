const http = require("http");
const port = process.env.PORT || 2345;
const { getCache, hasData } = require("./services/cacheService");

module.exports = http
  .createServer(async function(req, res) {
    res.writeHead(404, { "Content-Type": "text/plain" });

    const { url } = req;
    if (url.indexOf("/suggestions") === 0) {
      try {
        const suggestions = await getCache(url);
        send(res, suggestions);
      } catch (ex) {
        console.log(ex.message);
        res.end("An unexpected error occured.");
      }
    } else {
      res.end();
    }
  })
  .listen(port, "127.0.0.1");

function send(res, suggestions) {
  if (hasData(suggestions)) {
    res.writeHead(200, "OK", {
      "Content-Type": "application/json; charset=UTF-8"
    });
  }
  res.end(
    JSON.stringify({
      suggestions
    })
  );
}

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
