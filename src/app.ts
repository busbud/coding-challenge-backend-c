import http from "http";
var port = process.env.PORT || 2345;

export default http
  .createServer(function(req, res) {
    res.writeHead(404, { "Content-Type": "text/plain" });

    if (req.url.indexOf("/suggestions") === 0) {
      res.end(
        JSON.stringify({
          suggestions: [{ key: "example" }]
        })
      );
    } else {
      res.end();
    }
  })
  .listen(port, () => "127.0.0.1");

console.log("Server running at http://127.0.0.1:%d/suggestions", port);
