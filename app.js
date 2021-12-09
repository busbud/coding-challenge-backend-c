var http = require('http');
const url = require('url');
const suggestionsService = require('./src/suggestions')
var port = process.env.PORT || 2345;
//var host = process.env.HOST || '127.0.0.1';

module.exports = http.createServer(async function (req, res) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });


  if (req.url === '/') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      msg: "App is running"
    }));
  } else if (req.url.indexOf('/suggestions') === 0) {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    const params = url.parse(req.url, true).query
    console.log(params.q)
    if (!params.q) {
      res.writeHead(400, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({
        error: "Please add a q param to your request"
      }));
      return;
    }
    const suggestions = await suggestionsService.getSuggestions(params.q, params.latitude, params.longitude);
    if (suggestions.length === 0) {
      res.writeHead(404, { 'Content-Type': 'application/json' });
    }
    res.end(JSON.stringify({
      suggestions
    }));

  } else {
    res.end();
  }
}).listen(port, () => {
  console.log(`Server running at IP:${port}/suggestions`);

});
