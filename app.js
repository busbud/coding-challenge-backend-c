var http = require('http');
const querystring = require('querystring');
const url = require('url');

var port = process.env.PORT || 2345;

const Search = require('./src/suggestions')
const search = new Search();
module.exports = http.createServer(function (req, res) {
  let query = querystring.parse(url.parse(req.url).query)

  if (req.url.indexOf('/suggestions') === 0) {
    let suggestions = [];
    if (query.q !== undefined) {
      suggestions = search.findSuggest(query);
    }
    if (suggestions.length > 0) {
      res.writeHead(200, { 'Content-Type': 'application/json' });
    } else {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
    }
    res.end(JSON.stringify({
      suggestions,
    }));
  } else {
    res.end();
  }
}).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
