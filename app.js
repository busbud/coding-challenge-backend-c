var http = require('http');
var port = process.env.PORT || 2345;
var express = require('express');
var app = express();
var bodyParser = require('body-parser');
const url = require('url');
const querystring = require('querystring');


var fs = require('fs');

app.get('/suggestions', function(req, res) {
  var q = req.query.q
  var latitude = req.param('latitude');
  var longitude = req.param('longitude');

  res.send(q + ' ' + latitude + ' ' + longitude);
});

http.createServer(function (req, res) {
  fs.readFile('./data/cities_canada-us.json', function(err, data){
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.write(data);
    return res.end();
  });
  // if (req.url.indexOf('/suggestions') === 0) {

  //   res.end(


  //     );
  // } else {
  //   res.end();
  // }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);