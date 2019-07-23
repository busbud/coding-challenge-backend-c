var http = require('http');
const querystring = require('querystring');
const url = require('url');

var port = process.env.PORT || 2345;

const Search = require('./src/suggestions')
const search = new Search();
const cities = search.prepareData();
module.exports = http.createServer(function (req, res) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });
  let query = querystring.parse(url.parse(req.url).query)
  console.log(query);
  if (req.url.indexOf('/suggestions') === 0) {
    suggestions =
      res.end(JSON.stringify({
        suggestions: search.findSuggest(query)
      }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
