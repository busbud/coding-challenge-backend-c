var http = require('http');
var url = require('url');
var search = require('./search.js');
var Readable = require('stream').Readable;

var port = process.env.PORT || 2345;

// we replace JSON stringify by string chunks. For fun, it is made with streams, not sure if that part is optimum though
var resFormatter = function(items) {
  var readable = new Readable();
  readable._read = function noop() {console.log("_read")}; // https://stackoverflow.com/questions/12755997/how-to-create-streams-from-string-in-node-js
  readable.push('{"suggestions": [');
  readable.push('"' + items.join('","') + '"');
  readable.push(']}');
  readable.push(null);
  return readable;
}

// Server start
var server = http.createServer(function (req, res) {
  var urlData = url.parse(req.url, true); // Possibility of manual parsing of url, but lack of time for this. Not sure of performance of this.

  if (urlData.pathname === "/suggestions") { // indexOf === 0 was a problem in case of mismatch, it would have parsed the whole string

    if (!urlData.query.q) {

      res.writeHead(400, { 'Content-Type': 'text/plain' });
      res.end("Query is empty! Please add a 'q' query parameter to get suggestions");

    } else {

      res.writeHead(400, { 'Content-Type': 'text/json' });
      resFormatter(search.perform(urlData.query.q.toLowerCase()))
        // .pipe to gzip ? => Note: also needs a check of accept header
        .pipe(res);

    }
  } else {

    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end("Path not handled!");

  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port); // is listen synchronous ? is this log on the right place ?

module.exports = server;