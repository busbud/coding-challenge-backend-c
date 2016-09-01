var http = require('http');
var url = require('url');
var parse = require('csv-parse/lib/sync'); // we're gonna use syncronous method... it's slow, but we need the data before proceding anyway
var fs = require('fs');
var port = process.env.PORT || 2345;

let fileContents = fs.readFileSync(__dirname+'/data/cities_canada-usa.tsv');
var cities = parse(
  fileContents,
  {
    delimiter: "\t",
    quote: "",
    columns: true
  });

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    let params = url.parse(req.url, true).query
      , query = params.q
      , latitude = params.latitude || 0
      , longitude = params.longitude || 0
      , results = [];

    // magic will happen here

    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
