const http = require('http');
const port = process.env.PORT || 2345;
const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const url = require('url');
const querystring = require('querystring');
var suggestionsRouter = require('./api/suggestions');

app.use('/api/suggestions', suggestionsRouter);

function parseQuery(queryString) {
    var query = {};
    var pairs = (queryString[0] === '?' ? queryString.substr(1) : queryString).split('&');
    for (var i = 0; i < pairs.length; i++) {
        var pair = pairs[i].split('=');
        query[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1] || '');
    }
    console.log("console in parse function", query);
    return query;
}

http.createServer(function (req, res) {
  res.writeHead(200, {'Content-type':'text/plan'});


  if (req.url.indexOf('/suggestions') === 0) {
    // fs.readFile('./data/cities_canada-us.json', function(err, data){
    //   res.writeHead(200, {'Content-Type': 'text/plain'});
    //   res.write(data);
    //   return res.end();
    // });
     pathName= url.parse(req.url).pathname;
     query= url.parse(req.url).query;
     console.log('pathName1' + pathName);
     console.log('query2' + query);
     query = parseQuery(query);
     res.end(JSON.stringify(query));

  } else {
    res.end()
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);